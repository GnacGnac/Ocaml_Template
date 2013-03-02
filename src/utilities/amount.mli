
type t

val of_int : int -> t
val of_cent : int -> t

val of_string : string -> (t, [> `Not_an_amount of string]) Result.t

val to_string : t -> string

val zero : t
val add  : t -> t -> t
val sub  : t -> t -> t
val mul  : t -> t -> t
val div  : t -> t -> t
