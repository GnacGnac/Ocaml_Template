
open Result


module Occurence = struct

  type bound = Int of int | Infty

  type t = { min : int ; max : bound }

  let make min max = { min ; max }

  let min occurence = occurence.min
  let max occurence = occurence.max

  let le_bound a = function
    | Int i -> a <= i
    | Infty -> true

  let mem a occurence = (min occurence) <= a & le_bound a (max occurence)

  let none = make 0 (Int 0)

end


module type S = sig
  type node
  module NodeMap : Map_ext.S with type key = node
  type t
  val make : Occurence.t -> Occurence.t -> Occurence.t NodeMap.t -> t
  val check :
    node -> t -> int -> int -> int NodeMap.t ->
    (unit,
     [> `Bad_int_occurence of node * int * Occurence.t
      | `Bad_text_occurence of node * int * Occurence.t
      | `Bad_sub_node_occurence of node * node * int * Occurence.t]) Result.t
end


module Make (Node : Map_ext.ORDERED_TYPE) = struct

  type node = Node.t

  module NodeMap = Map_ext.Make (Node)

  type t =
      { ints : Occurence.t ;
	texts : Occurence.t ;
	sub_nodes : Occurence.t NodeMap.t }

  let make ints texts sub_nodes = { ints ; texts ; sub_nodes }

  let ints children_spec = children_spec.ints
  let texts children_spec = children_spec.texts
  let sub_nodes children_spec = children_spec.sub_nodes

  let add node occurence children_spec =
    let sub_nodes = NodeMap.add node occurence (sub_nodes children_spec) in
    make (ints children_spec) (texts children_spec) sub_nodes

  let merge_spec node occurence spec_occurence =
    match occurence, spec_occurence with
      | None, None -> None
      | Some occurence, None -> Some (occurence, Occurence.none)
      | None, Some spec_occurence -> Some (0, spec_occurence)
      | Some occurence, Some spec_occurence -> Some (occurence, spec_occurence)

  let check_sub_nodes_spec node nb_sub_nodes sub_nodes =
    let occurences_and_specs =
      NodeMap.merge merge_spec nb_sub_nodes sub_nodes in
    let f sub_node (occurence, spec_occurence) res =
      res >>= fun () ->
      if Occurence.mem occurence spec_occurence then return ()
      else
	error
	  (`Bad_sub_node_occurence (node, sub_node, occurence, spec_occurence))
    in
    NodeMap.fold f occurences_and_specs (return ())

  let check node children_spec nb_ints nb_texts nb_sub_nodes =
    let ints = ints children_spec in
    let texts = texts children_spec in
    let sub_nodes = sub_nodes children_spec in
    if Occurence.mem nb_ints ints then
      if Occurence.mem nb_texts texts then
	check_sub_nodes_spec node nb_sub_nodes sub_nodes
      else error (`Bad_text_occurence (node, nb_texts, texts))
    else
      error (`Bad_int_occurence (node, nb_ints, ints))

end
