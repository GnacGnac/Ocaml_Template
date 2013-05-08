
val to_string : string -> ('a -> string) -> 'a list -> string

val to_string_err :
  string -> ('a -> (string, 'b) Result.t) -> 'a list -> (string, 'b) Result.t

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val bind : ('a -> ('b, 'c) Result.t) -> 'a list -> ('b list, 'c) Result.t

val fold_bind :
  ('b -> 'a -> ('b, 'c) Result.t) -> 'b -> 'a list -> ('b, 'c) Result.t

val repeat : int -> 'a -> 'a list

val assoc : 'a -> ('a * 'b) list -> ('b, [> `Not_found]) Result.t
