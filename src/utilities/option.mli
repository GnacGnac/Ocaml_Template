
val map : ('a -> 'b) -> 'a option -> 'b option

(* Raises assertion failure on [None]. *)
val extract : 'a option -> 'a
