
type t

val text      : string -> t
val html      : t list -> t
val body      : t list -> t
val input     :
  ?typ:string -> ?value:string -> ?name:string -> ?size:int -> unit -> t
val font      : ?color:string -> ?face:string -> t list -> t
val bold      : t list -> t
val italic    : t list -> t
val paragraph : t list -> t
val center    : t list -> t
val br        : t
val table     :
  ?border:int -> ?cellpadding:int -> ?cellspacing:int -> t list -> t
val tr        : t list -> t
val td        : ?colspan:int -> ?rowspan:int -> t list -> t
val form      : ?method_:string -> ?action:string -> t list -> t
val spaces    : int -> t
val block     : t list -> t

val to_string : t -> string
