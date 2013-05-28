
open Result


module Generic = Block_generic
module Occurrence = Children_spec.Occurrence
module Primitive = Children_spec.Primitive

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

module Instance = Block_instance


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

  module type S = sig
    type t
    module type S = Instance.S with type Node.t = t
    val from_file :
      string ->
      ((module S),
       [> (node, node Position.t) parse_error
        | `Grammar_unrecognized_node of string Position.t]) Result.t
  end

  module Spec = struct

    include M

    let node_string =
      [(Grammar, "grammar") ; (Possible_roots, "possible_roots") ;
       (Children_specs, "children_specs") ; (Children_spec, "children_spec") ;
       (Name, "name") ; (Int, "int") ; (Text, "text") ; (Children, "children") ;
       (Child, "child") ; (Cardinality, "cardinality") ; (Min, "min") ;
       (Max, "max")]

    let possible_roots = [Grammar]

    let spec = function
      | Grammar ->
	(Primitive.none, ChildrenSpec.ones [Possible_roots ; Children_specs])
      | Possible_roots -> (Primitive.any_text, ChildrenSpec.empty)
      | Children_specs -> (Primitive.none, ChildrenSpec.any Children_spec)
      | Children_spec ->
	(Primitive.none,
	 [(Name, Occurrence.one) ; (Int, Occurrence.option) ;
	  (Text, Occurrence.option) ; (Children, Occurrence.option)])
      | Name ->	(Primitive.one_text, ChildrenSpec.empty)
      | Int -> (Primitive.none, ChildrenSpec.one Cardinality)
      | Text -> (Primitive.none, ChildrenSpec.one Cardinality)
      | Children -> (Primitive.none, ChildrenSpec.any Child)
      | Child -> (Primitive.none, ChildrenSpec.ones [Name ; Cardinality])
      | Cardinality ->
	(Primitive.none, [(Min, Occurrence.one) ; (Max, Occurrence.option)])
      | Min -> (Primitive.one_int, ChildrenSpec.empty)
      | Max -> (Primitive.one_int, ChildrenSpec.empty)

  end

  module G = Instance.MakeUnsafe (Spec)

  module type M = sig
    include String_ext.STRINGABLE
    val compare : t -> t -> int
  end

  module Make (M : M) = struct

    module type S = Instance.S with type Node.t = M.t

    type t = M.t

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
	| Primitive.Int -> int_cardinality
	| Primitive.Text -> text_cardinality in
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
	    Children.make Primitive.none Children.NodeMap.empty

      end in
      return (module Instance.Make (Spec) : S)

  end

  module MakeUnsafe (M : String_ext.UNSAFE_STRINGABLE) = struct

    module M' = struct

      type t = M.t
      let compare = Pervasives.compare

      let node_string =
	List.map (fun (x, y) -> (x, String.lowercase y)) M.node_string
      let string_node = List.map (fun (x, y) -> (y, x)) node_string

      let of_string s =
	map_error (fun `Not_found -> `Unrecognized_string s)
	  (List_ext.assoc (String.lowercase s) string_node)

      let to_string node = match List_ext.assoc node node_string with
	| Ok s -> s
	| Error _ ->
	  (* Should not happen. If so, check that every node is associated a
	     string in [node_string]. *)
	  assert false

    end

    include Make (M')

  end

  include G

end


module type PARSE_RESULT = sig
  include Instance.S
  val parse_result : (t, [> (Node.t, node_pos) parse_error]) Result.t
end

let parse_from_external unsafe_stringable file =
  let module M = (val unsafe_stringable : String_ext.UNSAFE_STRINGABLE) in
  let module Grammar = Grammar.MakeUnsafe (M) in
  match Grammar.from_file file with
  | Ok instance ->
    let module Instance = struct
      include (val instance : Grammar.S)
      let parse_result = parse file
    end in
    return (module Instance : PARSE_RESULT)
  | Error err -> error (`Grammar_error err)
