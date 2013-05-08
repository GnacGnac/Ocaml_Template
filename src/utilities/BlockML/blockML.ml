
open Result


module Generic = Block_generic
module Occurrence = Children_spec.Occurrence

type ('node, 'node_pos) occurrence_error =
  [ `Bad_int_occurrence of 'node_pos * int * Occurrence.t
  | `Bad_text_occurrence of 'node_pos * int * Occurrence.t
  | `Bad_sub_node_occurrence of 'node_pos * 'node * int * Occurrence.t]

type ('node, 'node_pos) analyze_error =
  [ ('node, 'node_pos) occurrence_error
  | `Not_a_root_node of 'node option Position.t]

type ('node, 'node_pos) parse_error =
  [ ('node, 'node_pos) analyze_error
  | `File_does_not_exist of string
  | `Could_not_open_file of string
  | `Unrecognized_char of char Position.t
  | `Unterminated_comment of unit Position.t
  | `Parse_error of unit Position.t
  | `Unrecognized_node of string Position.t]

module ChildrenSpec = Children_spec

module type STRINGABLE = sig
  type t
  val to_string : t -> string
  val of_string : string -> (t, [> `Unrecognized_string of string]) Result.t
end

module Instance = struct
  include Block_instance
  module type UNSAFESPEC = Block_unsafe_instance.SPEC
  module MakeUnsafe = Block_unsafe_instance.Make
end


module Grammar = struct

  type node =
    | Grammar
    | Possible_roots
    | Children_specs
    | Children_spec
    | Name
    | Int
    | Text
    | Children
    | Child
    | Cardinality
    | Min
    | Max

  module M = struct type t = node end

  module Spec = struct

    include M

    module Children = ChildrenSpec.MakeUnsafe (M)

    let node_string =
      [(Grammar, "grammar") ; (Possible_roots, "possible_roots") ;
       (Children_specs, "children_specs") ; (Children_spec, "children_spec") ;
       (Name, "name") ; (Int, "int") ; (Text, "text") ; (Children, "children") ;
       (Child, "child") ; (Cardinality, "cardinality") ; (Min, "min") ;
       (Max, "max")]

    let possible_roots = [Grammar]

    let spec = function
      | Grammar ->
	Children.make Children.Primitive.none
	  (Children.NodeMap.ones [Possible_roots ; Children_specs])
      | Possible_roots ->
	Children.make Children.Primitive.any_text Children.NodeMap.empty
      | Children_specs ->
	Children.make Children.Primitive.none
	  (Children.NodeMap.any Children_spec)
      | Children_spec ->
	Children.make Children.Primitive.none
	  (Children.NodeMap.of_list
	     [(Name, Occurrence.one) ;
	      (Int, Occurrence.option) ;
	      (Text, Occurrence.option) ;
	      (Children, Occurrence.option)])
      | Name ->
	Children.make Children.Primitive.one_text Children.NodeMap.empty
      | Int ->
	Children.make Children.Primitive.none (Children.NodeMap.one Cardinality)
      | Text ->
	Children.make Children.Primitive.none (Children.NodeMap.one Cardinality)
      | Children ->
	Children.make Children.Primitive.none (Children.NodeMap.any Child)
      | Child ->
	Children.make Children.Primitive.none
	  (Children.NodeMap.ones [Name ; Cardinality])
      | Cardinality ->
	Children.make Children.Primitive.none
	  (Children.NodeMap.ones [Min ; Max])
      | Min -> Children.make Children.Primitive.one_int Children.NodeMap.empty
      | Max -> Children.make Children.Primitive.one_int Children.NodeMap.empty

  end

  module G = Instance.MakeUnsafe (Spec)

  module Make (M : sig include STRINGABLE val compare : t -> t -> int end) =
  struct

    module type S = Instance.S with type Node.t = M.t

    module MSet = Set_ext.Make (M)
    module MMap = Map_ext.Make (M)
    module MChildren = ChildrenSpec.Make (M)

    let of_string pos_string =
      map_error
	(function
	| `Unrecognized_string _ -> `Grammar_unrecognized_node pos_string)
	(M.of_string (Position.contents pos_string))

    let possible_roots block =
      let possible_roots = G.extract_node Possible_roots block in
      let possible_roots = G.extract_text_children_with_pos possible_roots in
      List_ext.bind of_string possible_roots

    let cardinality_of_block block =
      let min_cardinality = G.extract_node Min block in
      let min_cardinality = G.extract_int min_cardinality in
      let max_cardinality = match G.get_node Max block with
	| Ok max_cardinality -> Occurrence.Int (G.extract_int max_cardinality)
	| Error `No_such_child -> Occurrence.Infty in
      Occurrence.make min_cardinality max_cardinality

    let extracted_cardinality_of_block block =
      cardinality_of_block (G.extract_node Cardinality block)

    let extract_cardinality_option block node =
      match G.get_node node block with
      | Ok cardinality -> extracted_cardinality_of_block cardinality
      | Error _ -> Occurrence.none

    let defined_children_spec_of_block block =
      let f children_map child =
	let name = G.extract_text_with_pos (G.extract_node Name child) in
	of_string name >>= fun name ->
	let cardinality = extracted_cardinality_of_block child in
	return (MChildren.NodeMap.add name cardinality children_map) in
      List_ext.fold_bind f MChildren.NodeMap.empty
	(G.extract_node_children Child block)

    let children_spec_of_block block = match G.get_node Children block with
      | Ok children -> defined_children_spec_of_block children
      | Error `No_such_child -> return MChildren.NodeMap.empty

    let add_children_spec children_spec block =
      let name = G.extract_text_with_pos (G.extract_node Name block) in
      of_string name >>= fun name ->
      let int_cardinality = extract_cardinality_option block Int in
      let text_cardinality = extract_cardinality_option block Text in
      let primitives = function
	| MChildren.Primitive.Int -> int_cardinality
	| MChildren.Primitive.Text -> text_cardinality in
      children_spec_of_block block >>= fun children_map ->
      let added_spec = MChildren.make primitives children_map in
      return (MMap.add name added_spec children_spec)

    let children_spec block =
      let children_specs = G.extract_node Children_specs block in
      let children_specs =
	G.extract_node_children Children_spec children_specs in
      List_ext.fold_bind add_children_spec MMap.empty children_specs

    let from_file file =
      G.parse file >>= fun block ->
      possible_roots block >>= fun possible_roots ->
      children_spec block >>= fun children_spec ->
      let module Spec = struct

	include M

	module Set = MSet
	module Map = MMap

	module Children = MChildren

	let possible_roots = Set.of_list possible_roots

	let spec node = match Map.find node children_spec with
	  | Ok children_spec -> children_spec
	  | Error `Not_found ->
	    Children.make Children.Primitive.none Children.NodeMap.empty

      end in
      return (module Instance.Make (Spec) : S)

  end

  include G

end
