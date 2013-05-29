
module type TO_STRING = sig
  type t
  val to_string : t -> string
end

module type OF_STRING = sig
  type t
  val of_string : string -> (t, [> `Unrecognized_string of string]) Result.t
end

module type STRINGABLE = sig
  type t
  val to_string : t -> string
  val of_string : string -> (t, [> `Unrecognized_string of string]) Result.t
end

module type UNSAFE_STRINGABLE = sig
  type t
  val node_string : (t * string) list
end

module Set : Set_ext.S with type elt = string

module Map : Map_ext.S with type key = string

val repeat : int -> string -> string

val remove_chars : string -> char list -> string
