
type not_found = [`Not_found]

module type S = sig
  type a
  type b
  type t
  val compare : t -> t -> int
  val empty : t
  val add : a -> b -> t -> t
  val find1 : a -> t -> (b, [> not_found]) Result.t
  val find2 : b -> t -> (a, [> not_found]) Result.t
  val mem2 : b -> t -> bool
  val of_list : (a * b) list -> t
end

module Make (A : Map_ext.ORDERED_TYPE) (B : Map_ext.ORDERED_TYPE) :
  S with type a = A.t and type b = B.t
