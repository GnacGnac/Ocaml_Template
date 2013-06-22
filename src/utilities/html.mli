
module type PARAMETER = sig
  module Name : String_ext.TO_STRING
  module Action : String_ext.TO_STRING
end

module type UNSAFE_PARAMETER = sig
  module Name : String_ext.UNSAFE_STRINGABLE
  module Action : String_ext.UNSAFE_STRINGABLE
end

module type S = sig
  type name
  type action
  type t
  type html = t

  type method_ = Get | Post
  type type_ = Text | Password | Text_area | Submit | Checkbox | Hidden
  type color = Rgb of int * int * int
  val red : color
  type face = Arial
  type selected = Selected_value

  val string : string -> string

  val text        : string -> t
  val exact_text  : string -> t
  val html        : t list -> t
  val body        : t list -> t
  val input       :
    ?type_:type_ -> ?value:string -> ?name:name -> ?size:int -> unit -> t
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
  val select      : ?name:name -> t list -> t
  val option      : ?selected:selected -> ?value:string -> t list -> t
  val strike      : t list -> t
  val a           : ?href:action -> t list -> t

  val to_string : t -> string

  module Button : sig
    type t
    val make : string -> name -> t
  end

  module EditableInfos : sig
    type t
    val make :
      line_add_cells:(html list) ->
      add_button:Button.t -> edit_button:Button.t ->
      edit_selected:name -> edit_options:((string * name) list) ->
      cell_id:(int -> name) -> t
  end

  val result_table :
    ?border:int -> ?cellpadding:int -> ?cellspacing:int ->
    string -> string list -> ?editable_infos:EditableInfos.t -> t list list -> t
end

module Make (Parameter : PARAMETER) :
  S with type name = Parameter.Name.t and type action = Parameter.Action.t

module MakeUnsafe (Parameter : UNSAFE_PARAMETER) :
  S with type name = Parameter.Name.t and type action = Parameter.Action.t
