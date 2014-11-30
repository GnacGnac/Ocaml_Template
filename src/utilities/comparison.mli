
module type M = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type t
  val compare : t -> t -> int
  val (<=) : t -> t -> bool
  val (>=) : t -> t -> bool
  val (<) : t -> t -> bool
  val (>) : t -> t -> bool
end

module Make (M : M) : S with type t = M.t
