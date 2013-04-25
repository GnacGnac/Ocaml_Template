
open Result


module Generic = Block_generic
module Occurrence = Children_spec.Occurrence
module ChildrenSpec = Children_spec
module Instance = Block_instance
module UnsafeInstance = Block_unsafe_instance


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
  | `Parse_error of unit Position.t
  | `Unrecognized_node of string Position.t]


type grammar_node =
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

module Grammar = struct

  module M = struct type t = grammar_node end

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
	Children.make Children.no_primitive
	  (Children.NodeMap.ones [Possible_roots ; Children_specs])
      | Possible_roots ->
	Children.make
	  (Children.any_primitive Children.Text) Children.NodeMap.empty
      | Children_specs ->
	Children.make Children.no_primitive (Children.NodeMap.any Children_spec)
      | Children_spec ->
	Children.make Children.no_primitive
	  (Children.NodeMap.of_list
	     [(Name, Occurrence.one) ;
	      (Int, Occurrence.option) ;
	      (Text, Occurrence.option) ;
	      (Children, Occurrence.option)])
      | Name ->
	Children.make Children.no_primitive Children.NodeMap.empty
      | Int ->
	Children.make Children.no_primitive (Children.NodeMap.one Cardinality)
      | Text ->
	Children.make Children.no_primitive (Children.NodeMap.one Cardinality)
      | Children ->
	Children.make Children.no_primitive (Children.NodeMap.any Child)
      | Child ->
	Children.make Children.no_primitive
	  (Children.NodeMap.ones [Name ; Cardinality])
      | Cardinality ->
	Children.make Children.no_primitive
	  (Children.NodeMap.ones [Min ; Max])
      | Min ->
	Children.make (Children.one_primitive Children.Int)
	  Children.NodeMap.empty
      | Max ->
	Children.make (Children.one_primitive Children.Int)
	  Children.NodeMap.empty

  end

  include UnsafeInstance.Make (Spec)

end

let from_file file =
  Grammar.parse file >>= fun block ->
  let module Spec = struct

    type t = string

    let to_string s = s

    module Set = String_ext.Set

    module Children = ChildrenSpec.Make (String)

    let possible_roots =
      let possible_roots = Grammar.extract_child_node Possible_roots block in
      let f_possible_root possible_root =
	Grammar.extract_text [possible_root] in
      Set.of_list (List.map f_possible_root possible_roots)

    let cardinality_of_block block =
      let min_cardinality = Grammar.extract_child_node Min block in
      let min_cardinality = Grammar.extract_int min_cardinality in
      let max_cardinality = Grammar.extract_child_node Max block in
      assert false

    let add_children_spec (nodes, children_spec) block =
      let name =
	Grammar.extract_text (Grammar.extract_child_node Name block) in
(*
      let child =
	Grammar.extract_text (Grammar.extract_child_node Parent block) in
*)
      assert false

    let spec _ = assert false

    let of_string _ = assert false

  end in
  return (module Instance.Make (Spec) : Instance.S)
