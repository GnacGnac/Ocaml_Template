
type ('a, 'b) t = Ok of 'a | Error of 'b

val return : 'a -> ('a, 'b) t
val error : 'b -> ('a, 'b) t

val (>>=) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

val map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
