
type unrecognized_string = [`Unrecognized_string of string]
type not_an_int = [`Not_an_int of string]

module type TO_STRING = sig
  type t
  val to_string : t -> string
end

module type OF_STRING = sig
  type t
  val of_string : string -> (t, [> unrecognized_string]) Result.t
end

module type STRINGABLE = sig
  type t
  val to_string : t -> string
  val of_string : string -> (t, [> unrecognized_string]) Result.t
end

module type UNSAFE_STRINGABLE = sig
  type t
  val string_assoc : (t * string) list
end

module MakeStringable (UnsafeStringable : UNSAFE_STRINGABLE) :
  STRINGABLE with type t = UnsafeStringable.t

module Set : Set_ext.S with type elt = string

module Map : Map_ext.S with type key = string

val repeat : int -> string -> string

val remove_chars : string -> char list -> string

val parse_indices : string -> int -> string -> int list option
val add_indices : string -> int list -> string

val to_int : string -> (int, [> not_an_int]) Result.t

val uppercase_first : string -> string

val contains : string -> ?from:int -> string -> (int * int) option

val to_list : string -> char list
