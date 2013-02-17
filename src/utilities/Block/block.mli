
type t
type child
type children = child list

val node : string -> children -> t
val block : t -> child
val text : string -> child

val to_string : t -> string
