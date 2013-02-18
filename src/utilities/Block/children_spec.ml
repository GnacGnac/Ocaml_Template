
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

  let check node children_spec nb_ints nb_texts nb_sub_nodes =
    return () (* TODO *)

end
