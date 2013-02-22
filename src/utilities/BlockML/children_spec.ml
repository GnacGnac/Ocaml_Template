
open Result


module Occurrence = struct

  type bound = Int of int | Infty

  let string_of_bound = function
    | Int i -> string_of_int i
    | Infty -> "*"

  type t = { min : int ; max : bound }

  let make min max = { min ; max }

  let min occurrence = occurrence.min
  let max occurrence = occurrence.max

  let le_bound a = function
    | Int i -> a <= i
    | Infty -> true

  let mem a occurrence = (min occurrence) <= a & le_bound a (max occurrence)

  let exactly i = make i (Int i)
  let at_least i = make i Infty
  let at_most i = make 0 (Int i)
  let between i j = make i (Int j)
  let none = exactly 0
  let any = at_least 0

  let to_string occurrence =
    Printf.sprintf "%d..%s" (min occurrence) (string_of_bound (max occurrence))

end


module type S = sig
  type node
  type node_pos = node Position.t
  module NodeMap : Map_ext.S with type key = node
  type t
  val make : Occurrence.t -> Occurrence.t -> Occurrence.t NodeMap.t -> t
  val check :
    node_pos -> t -> int -> int -> int NodeMap.t ->
    (unit,
     [> `Bad_int_occurrence of node_pos * int * Occurrence.t
      | `Bad_text_occurrence of node_pos * int * Occurrence.t
      | `Bad_sub_node_occurrence of
	  node_pos * node * int * Occurrence.t]) Result.t
end


module Make (Node : Map_ext.ORDERED_TYPE) = struct

  type node = Node.t

  type node_pos = node Position.t

  module NodeMap = Map_ext.Make (Node)

  type t =
      { ints : Occurrence.t ;
	texts : Occurrence.t ;
	sub_nodes : Occurrence.t NodeMap.t }

  let make ints texts sub_nodes = { ints ; texts ; sub_nodes }

  let ints children_spec = children_spec.ints
  let texts children_spec = children_spec.texts
  let sub_nodes children_spec = children_spec.sub_nodes

  let add node occurrence children_spec =
    let sub_nodes = NodeMap.add node occurrence (sub_nodes children_spec) in
    make (ints children_spec) (texts children_spec) sub_nodes

  let merge_spec node occurrence spec_occurrence =
    match occurrence, spec_occurrence with
      | None, None -> None
      | Some occurrence, None -> Some (occurrence, Occurrence.none)
      | None, Some spec_occurrence -> Some (0, spec_occurrence)
      | Some occurrence, Some spec_occurrence ->
	Some (occurrence, spec_occurrence)

  let check_sub_nodes_spec node nb_sub_nodes sub_nodes =
    let occurrences_and_specs =
      NodeMap.merge merge_spec nb_sub_nodes sub_nodes in
    let f sub_node (occurrence, spec_occurrence) res =
      res >>= fun () ->
      if Occurrence.mem occurrence spec_occurrence then return ()
      else
	error
	  (`Bad_sub_node_occurrence
	      (node, sub_node, occurrence, spec_occurrence))
    in
    NodeMap.fold f occurrences_and_specs (return ())

  let check node children_spec nb_ints nb_texts nb_sub_nodes =
    let ints = ints children_spec in
    let texts = texts children_spec in
    let sub_nodes = sub_nodes children_spec in
    if Occurrence.mem nb_ints ints then
      if Occurrence.mem nb_texts texts then
	check_sub_nodes_spec node nb_sub_nodes sub_nodes
      else error (`Bad_text_occurrence (node, nb_texts, texts))
    else
      error (`Bad_int_occurrence (node, nb_ints, ints))

end
