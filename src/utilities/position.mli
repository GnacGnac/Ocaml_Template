
type 'a t

val make : 'a -> int -> int -> 'a t
val make_dummy : 'a -> 'a t

val contents : 'a t -> 'a
val line : 'a t -> (int, [> `No_position]) Result.t
val char : 'a t -> (int, [> `No_position]) Result.t

val change_contents : 'a -> 'b t -> 'a t

val apply : (int -> int -> 'a -> 'b) -> ('a -> 'b) -> 'a t -> 'b
val map_contents : ('a -> 'b) -> 'a t -> 'b t
