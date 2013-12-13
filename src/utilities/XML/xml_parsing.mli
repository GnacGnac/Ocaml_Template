
type attribute = string Position.t * string Position.t
type attributes = attribute list
type t

val make : string Position.t -> string Position.t -> attributes -> t list -> t
val start_tag : t -> string Position.t
val end_tag : t -> string Position.t
val attributes : t -> attributes
val children : t -> t list
