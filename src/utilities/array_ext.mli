
val make_matrix : int -> int -> 'a -> 'a array array

val foldi : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a

type out_of_bounds = [`Out_of_bounds]

val get : 'a array -> int -> ('a, out_of_bounds) Result.t
