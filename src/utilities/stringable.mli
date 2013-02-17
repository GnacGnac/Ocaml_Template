
module type ASSOC = sig
  type t
  val compare : t -> t -> int
  val get : (t * string) list
end

module type S = sig
  type t
  val of_string :
    string -> (t, [> `Unrecognized_string_assoc of string]) Result.t
  val mem       : string -> bool
  val to_string : t -> (string, [> `Unrecognized_elt_assoc of t]) Result.t
  module Set : Set_ext.S with type elt = t
  val all : Set.t
end

module Make (A : ASSOC) : S with type t = A.t
