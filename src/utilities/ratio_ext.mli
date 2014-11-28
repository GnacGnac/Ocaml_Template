
type t

val of_int : int -> t

val is_null : t -> bool
val zero : t
val add  : t -> t -> t
val sub  : t -> t -> t
val mul  : t -> t -> t
val div  : t -> t -> (t, [`Division_by_zero]) Result.t

val to_string : t -> string
