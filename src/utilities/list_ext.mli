
type not_found = [`Not_found]
type empty_list = [`Empty_list]
type out_of_bounds = [`Out_of_bounds]

val insert_between : 'a -> 'a list -> 'a list

val to_string : string -> ('a -> string) -> 'a list -> string

val to_string_err :
  string -> ('a -> (string, 'b) Result.t) -> 'a list -> (string, 'b) Result.t

val foldi : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val bind : ('a -> ('b, 'c) Result.t) -> 'a list -> ('b list, 'c) Result.t

val fold_bind :
  ('b -> 'a -> ('b, 'c) Result.t) -> 'b -> 'a list -> ('b, 'c) Result.t

val repeat : int -> 'a -> 'a list

val assoc : 'a -> ('a * 'b) list -> ('b, [> not_found]) Result.t

val removei : int -> 'a list -> 'a list

val map_nth : int -> ('a -> 'a) -> 'a list -> 'a list

val make : int -> 'a -> 'a list

val find_and_apply :
  ('a -> 'b option) -> 'a list -> ('b, [> not_found]) Result.t

val nth : 'a list -> int -> ('a, [> out_of_bounds]) Result.t

val make_with_next : 'a -> ('a -> 'a) -> int -> 'a list

val index_of : 'a -> 'a list -> (int, [> not_found]) Result.t

val filter_and_apply : ('a -> 'b option) -> 'a list -> 'b list

val repeat_rank : 'a list -> 'a list

val pick : 'a list -> ('a * 'a list, [> empty_list]) Result.t

val product : 'a list -> 'b list -> ('a * 'b) list

val remove_doubles : ('a -> 'a -> bool) -> 'a list -> 'a list

val removes : 'a list -> 'a list -> 'a list

val lex_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

val hd : 'a list -> ('a, empty_list) Result.t

val last : 'a list -> ('a, empty_list) Result.t
