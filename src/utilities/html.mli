
type t
type html = t

val string : string -> string

val text        : string -> t
val text_string : string -> t
val html        : t list -> t
val body        : t list -> t
val input       :
  ?type_:string -> ?value:string -> ?name:string -> ?size:int -> unit -> t
val font        : ?color:string -> ?face:string -> t list -> t
val bold        : t list -> t
val italic      : t list -> t
val paragraph   : t list -> t
val center      : t list -> t
val br          : t
val table       :
  ?border:int -> ?cellpadding:int -> ?cellspacing:int -> t list -> t
val tr          : ?bgcolor:string -> t list -> t
val td          : ?colspan:int -> ?rowspan:int -> t list -> t
val form        : ?method_:string -> ?action:string -> t list -> t
val space       : t
val spaces      : int -> t
val block       : t list -> t
val select      : t list -> t
val option      : ?selected:string -> ?value:string -> t list -> t

val to_string : t -> string

module EditableInfos : sig
  type t
  val make :
    html list -> string -> string -> string -> string ->
    (string * string) list -> t
end

val result_table :
  ?border:int -> ?cellpadding:int -> ?cellspacing:int ->
  string -> string -> (int -> string) -> string -> string list ->
  ?editable_infos:EditableInfos.t -> t list list -> t
