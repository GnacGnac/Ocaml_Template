
type t

val make : int -> int -> t

val line : t -> int
val char : t -> int

val of_buffer : Lexing.lexbuf -> t
