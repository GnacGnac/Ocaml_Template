
val make_matrix : int -> int -> 'a -> 'a array array

val foldi : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a

val get : 'a array -> int -> ('a, [> `Out_of_bounds]) Result.t
