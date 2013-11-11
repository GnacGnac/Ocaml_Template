
val map : ('a -> 'b) -> 'a option -> 'b option

(* Raises assertion failure on [None]. *)
val extract : 'a option -> 'a

val of_result : ('a, 'b) Result.t -> 'a option

val fold : 'a -> ('b -> 'a) -> 'b option -> 'a
