
type ('a, 'b) t = Ok of 'a | Error of 'b

val return : 'a -> ('a, 'b) t
val error : 'b -> ('a, 'b) t

val (>>=) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
val (|>>) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t

val map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
val map_result : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val fold : ('b -> 'a) -> ('c -> 'a) -> ('b, 'c) t -> 'a
val get : ('b -> 'a) -> ('a, 'b) t -> 'a

val to_option : ('a, 'b) t -> 'a option
val of_option : 'b -> 'a option -> ('a, 'b) t

val safe_try : (unit -> 'a) -> (exn -> 'b) -> ('a, 'b) t

(* Only use on valid monads. *)
val extract : ('a, 'b) t -> 'a

val (|>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val (@@) : ('a -> 'b) -> 'a -> 'b
val (|-) : 'a -> ('a -> 'b) -> 'b
val fun_pow : ('a -> 'a) -> int -> 'a -> 'a
