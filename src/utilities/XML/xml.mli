
module Attributes : sig
  type t
  val empty : t
  val get_pos : t -> string -> (unit Position.t, [> `Not_found]) Result.t
  val extract_pos : t -> string -> unit Position.t
  val get : t -> string -> (string, [> `Not_found]) Result.t
  val get_with_pos : t -> string -> (string Position.t, [> `Not_found]) Result.t
  val extract : t -> string -> string
  val extract_with_pos : t -> string -> string Position.t
  val add : t -> string -> string -> (t, [> `Attribute_already_exists]) Result.t
  val add_with_pos :
    t -> string Position.t -> string Position.t ->
    (t, [> `Attribute_already_exists]) Result.t
  val update : t -> string -> string -> t
  val update_with_pos : t -> string Position.t -> string Position.t -> t
  val remove : t -> string -> t
  val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_with_pos :
    (string Position.t -> string Position.t -> 'a -> 'a) -> t -> 'a -> 'a
end

type t

val node : string -> Attributes.t -> t list -> t
val node_with_pos : string Position.t -> Attributes.t -> t list -> t

val get_node : t -> string
val get_node_with_pos : t -> string Position.t
val get_attributes : t -> Attributes.t
val get_attribute : t -> string -> (string, [> `Not_found]) Result.t
val get_attribute_with_pos :
  t -> string -> (string Position.t, [> `Not_found]) Result.t
val extract_attribute : t -> string -> string
val extract_attribute_with_pos : t -> string -> string Position.t
val get_children : t -> t list
val get_node_children : t -> string -> t list
val get_node_child : t -> string -> (t, [> `Not_found]) Result.t
val extract_node_child : t -> string -> t

val from_file :
  string ->
  (t,
   [> `Could_not_open_file of string
    | `File_does_not_exist of string
    | `Parse_error of unit Position.t
    | `Unrecognized_char of char Position.t
    | `Unterminated_comment of unit Position.t
    | `Different_opening_and_closing_tags of
	string Position.t * string Position.t]) Result.t
