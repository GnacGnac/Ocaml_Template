
module Occurence : sig
  type bound = Int of int | Infty
  type t
  val make : int -> bound -> t
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

module Make (Node : Map_ext.ORDERED_TYPE) : S with type node = Node.t
