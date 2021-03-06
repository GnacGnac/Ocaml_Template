
type t

val to_int : t -> (int, [> `Not_an_int of t]) Result.t
val of_int : int -> t

val of_string : string -> (t, [> `Not_an_amount of string]) Result.t

val to_string : ?sep : string -> t -> string

val is_null : t -> bool
val zero : t
val add  : t -> t -> t
val sub  : t -> t -> t
val mul  : t -> t -> t
val div  : t -> t -> (t, [`Division_by_zero]) Result.t
