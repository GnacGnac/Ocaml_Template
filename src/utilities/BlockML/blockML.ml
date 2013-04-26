
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
	Children.make
	  (Children.one_primitive Children.Text) Children.NodeMap.empty
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
    module Map = String_ext.Map

    module Children = ChildrenSpec.Make (String)

    let possible_roots =
      let possible_roots = Grammar.extract_node Possible_roots block in
      Set.of_list (Grammar.extract_text_children possible_roots)

    let cardinality_of_block block =
      let min_cardinality = Grammar.extract_node Min block in
      let min_cardinality = Grammar.extract_int min_cardinality in
      let max_cardinality = match Grammar.get_node Max block with
	| Ok max_cardinality ->
	  Occurrence.Int (Grammar.extract_int max_cardinality)
	| Error `No_such_child -> Occurrence.Infty in
      Occurrence.make min_cardinality max_cardinality

    let extracted_cardinality_of_block block =
      cardinality_of_block (Grammar.extract_node Cardinality block)

    let extract_cardinality_option block node =
      match Grammar.get_node node block with
	| Ok cardinality -> extracted_cardinality_of_block cardinality
	| Error _ -> Occurrence.none

    let defined_children_spec_of_block block =
      let f (nodes, children_map) child =
	let name = Grammar.extract_text (Grammar.extract_node Name child) in
	let cardinality = extracted_cardinality_of_block child in
	let nodes = Set.add name nodes in
	let children_map = Children.NodeMap.add name cardinality children_map in
	(nodes, children_map) in
      List.fold_left f (Set.empty, Children.NodeMap.empty)
	(Grammar.extract_node_children Child block)

    let children_spec_of_block block =
      match Grammar.get_node Children block with
	| Ok children -> defined_children_spec_of_block children
	| Error `No_such_child -> (Set.empty, Children.NodeMap.empty)

    let add_children_spec (nodes, children_spec) block =
      let name = Grammar.extract_text (Grammar.extract_node Name block) in
      let int_cardinality = extract_cardinality_option block Int in
      let text_cardinality = extract_cardinality_option block Text in
      let primitives = function
	| Children.Int -> int_cardinality
	| Children.Text -> text_cardinality in
      let (added_nodes, children_map) = children_spec_of_block block in
      let added_nodes = Set.add name added_nodes in
      let added_spec = Children.make primitives children_map in
      (Set.union nodes added_nodes, Map.add name added_spec children_spec)

    let (nodes, children_spec) =
      let children_specs = Grammar.extract_node Children_specs block in
      let children_specs =
	Grammar.extract_node_children Children_spec children_specs in
      List.fold_left add_children_spec (Set.empty, Map.empty) children_specs

    let spec node = match Map.find node children_spec with
      | Ok children_spec -> children_spec
      | Error `Not_found ->
	Children.make Children.no_primitive Children.NodeMap.empty

    let of_string node =
      if Set.mem node nodes then return node
      else error (`Unrecognized_string node)

  end in
  return (module Instance.Make (Spec) : Instance.S)
