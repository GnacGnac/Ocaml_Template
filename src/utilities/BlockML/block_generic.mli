
module type NODE = sig
  type t
  val to_string : t -> (string, [> `Node_not_bound_to_a_string of t]) Result.t
end

module type S = sig
  module Node : NODE
  type t = Int of int | Text of string | Node of Node.t * t list
  val int : int -> t
  val text : string -> t
  val node : Node.t -> t list -> t
  val to_string :
    t -> (string, [> `Node_not_bound_to_a_string of Node.t]) Result.t
end

module Make (N : NODE) : S with type Node.t = N.t
