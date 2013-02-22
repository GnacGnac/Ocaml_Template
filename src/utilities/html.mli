
type t

val text      : string -> t
val html      : t list -> t
val body      : t list -> t
val input     : ?typ:string -> ?text:string -> unit -> t
(*
val font      : t list -> t
val bold      : t list -> t
val italic    : t list -> t
val paragraph : t list -> t
val table     :
  border:int -> ?cellpadding:int -> ?cellspacing:int -> t list -> t
val tr        : make_node
val td        : ?colspan:int -> ?rowspan:int -> t list -> t
val center    : t list -> t
val br        : t
*)

val to_string : t -> string
