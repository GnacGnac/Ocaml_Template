
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
  | Cardinalities
  | Cardinality
  | Parent
  | Child
  | Min
  | Max
  | Unlimited


module Grammar = struct

  module M = struct type t = grammar_node end

  module Spec = struct

    include M

    module Children = ChildrenSpec.MakeUnsafe (M)

    let node_string =
      [(Grammar, "grammar") ; (Possible_roots, "possible_roots") ;
       (Cardinalities, "cardinalities") ; (Cardinality, "cardinality") ;
       (Parent, "parent") ; (Child, "child") ; (Min, "min") ; (Max, "max") ;
       (Unlimited, "unlimited")]

    let possible_roots = [Grammar]

    let spec = function
      | Grammar ->
	Children.make Occurrence.none Occurrence.none
	  (Children.NodeMap.of_list [(Possible_roots, Occurrence.one) ;
				     (Cardinalities, Occurrence.one)])
      | Possible_roots ->
	Children.make Occurrence.none Occurrence.any Children.NodeMap.empty
      | Cardinalities ->
	Children.make Occurrence.none Occurrence.none
	  (Children.NodeMap.singleton Cardinality Occurrence.any)
      | Cardinality ->
	Children.make Occurrence.none Occurrence.none
	  (Children.NodeMap.of_list
	     (List.map (fun x -> (x, Occurrence.one))
		[Parent ; Child ; Min ; Max]))
      | Parent ->
	Children.make Occurrence.none Occurrence.one Children.NodeMap.empty
      | Child ->
	Children.make Occurrence.none Occurrence.one Children.NodeMap.empty
      | Min ->
	Children.make Occurrence.one Occurrence.none Children.NodeMap.empty
      | Max ->
	Children.make Occurrence.option Occurrence.none
	  (Children.NodeMap.singleton Unlimited Occurrence.option)
      | Unlimited ->
	Children.make Occurrence.none Occurrence.none Children.NodeMap.empty

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

    let possible_roots = assert false

    let spec _ = assert false

    let of_string _ = assert false

  end in
  return (module Instance.Make (Spec) : Instance.S)
