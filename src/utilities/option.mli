
val map : ('a -> 'b) -> 'a option -> 'b option

(* Raises assertion failure on [None]. *)
val extract : 'a option -> 'a

val of_result : ('a, 'b) Result.t -> 'a option
val to_result : 'b -> 'a option -> ('a, 'b) Result.t

val fold : 'a -> ('b -> 'a) -> 'b option -> 'a
val get : 'a -> 'a option -> 'a
