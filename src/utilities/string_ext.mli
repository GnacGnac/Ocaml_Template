
module Set : Set_ext.S with type elt = string

module Map : Map_ext.S with type key = string

val repeat : int -> string -> string

val remove_chars : string -> char list -> string
