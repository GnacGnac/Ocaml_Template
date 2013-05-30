
module type PARAMETER = sig
  module Name : String_ext.TO_STRING
  module Value : String_ext.TO_STRING
  module Action : String_ext.TO_STRING
end

module type UNSAFE_PARAMETER = sig
  module Name : String_ext.UNSAFE_STRINGABLE
  module Value : String_ext.UNSAFE_STRINGABLE
  module Action : String_ext.UNSAFE_STRINGABLE
end

module type S = sig
  type name
  type value
  type action
  type t
  type html = t

  type method_ = Get | Post
  type type_ = Text | Password | Text_area | Submit | Checkbox
  type color = Rgb of int * int * int
  type face = Arial
  type selected = Selected_value

  val string : string -> string

  val text        : string -> t
  val text_string : string -> t
  val html        : t list -> t
  val body        : t list -> t
  val input       :
    ?type_:type_ -> ?value:value -> ?name:name -> ?size:int -> unit -> t
  val font        : ?color:color -> ?face:face -> t list -> t
  val bold        : t list -> t
  val italic      : t list -> t
  val paragraph   : t list -> t
  val center      : t list -> t
  val br          : t
  val table       :
    ?border:int -> ?cellpadding:int -> ?cellspacing:int -> t list -> t
  val tr          : ?bgcolor:color -> t list -> t
  val td          : ?colspan:int -> ?rowspan:int -> t list -> t
  val form        : ?method_:method_ -> ?action:action -> t list -> t
  val space       : t
  val spaces      : int -> t
  val block       : t list -> t
  val select      : t list -> t
  val option      : ?selected:selected -> ?value:value -> t list -> t

  val to_string : t -> string

  module EditableInfos : sig
    type t
    val make :
      html list -> value -> name -> value -> name ->
      (string * value) list -> t
  end

  val result_table :
    ?border:int -> ?cellpadding:int -> ?cellspacing:int ->
    action -> string -> (int -> name) -> method_ -> string list ->
    ?editable_infos:EditableInfos.t -> t list list -> t
end

module Make (Parameter : PARAMETER) :
  S with
      type name = Parameter.Name.t and
      type value = Parameter.Value.t and
      type action = Parameter.Action.t

module MakeUnsafe (Parameter : UNSAFE_PARAMETER) :
  S with
      type name = Parameter.Name.t and
      type value = Parameter.Value.t and
      type action = Parameter.Action.t
