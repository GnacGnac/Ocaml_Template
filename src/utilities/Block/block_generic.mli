
module type NODE = sig
  type t
  val to_string : t -> (string, [> `Node_not_bound_to_a_string of t]) Result.t
end

module type S = sig
  module Node : NODE
  type t
  type child = Block of t | Int of int | Text of string
  type children = child list
  val node : Node.t -> children -> t
  val block : t -> child
  val int : int -> child
  val text : string -> child
  val to_string :
    t -> (string, [> `Node_not_bound_to_a_string of Node.t]) Result.t
end

module Make (N : NODE) : S with type Node.t = N.t
