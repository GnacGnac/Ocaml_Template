
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
  type color = Rgb of int * int * int | Transparent
  val bg_main : color
  val bg_title : color
  val bg1 : color
  val bg2 : color
  val bg_submit : color
  val red : color
  val blue : color
  val transparent : color
  type face = Arial
  type font_family = Face of face
  type alignment = Left | Right | Center | Top | Down | Justify
  type size = Percent of int | Pixel of int | Absolute of int | Auto | Em of int
  type cursor = Pointer
  type position = Absolute
  type list_style_type = No_list_style_type
  type class_name =
  | Class_node of string
  | Sub_class of string * string
  | New_class of string
  type style_attribute =
  | Text_align of alignment
  | Color of color
  | Background_color of color
  | Cursor of cursor
  | Border of size
  | Margin of size
  | Padding of size
  | Font_size of size
  | Font_family of font_family list
  | Position of position
  | List_style_type of list_style_type
  | Alignment of (alignment * size)
  type style_attributes = style_attribute list

  type 'a attribute_node = ?class_:string -> ?style:style_attributes -> 'a

  val string : string -> string

  val text        : string -> t
  val exact_text  : string -> t
  val html        : (t list -> t) attribute_node
  val head        : (t list -> t) attribute_node
  val body        : (t list -> t) attribute_node
  val title       : (t list -> t) attribute_node
  val div         : (t list -> t) attribute_node
  val span        : (t list -> t) attribute_node
  val strong      : (t list -> t) attribute_node
  val em          : (t list -> t) attribute_node
  val paragraph   : (t list -> t) attribute_node
  val br          : (unit -> t) attribute_node
  val space       : t
  val spaces      : int -> t
  val form        :
    (?method_:method_ -> ?action:action -> ?onsubmit:string -> ?name:name ->
     t list -> t) attribute_node
  val input       :
    (?type_:type_ -> ?value:string -> ?name:name -> unit -> t) attribute_node
  val a           : (?href:string -> t list -> t) attribute_node
  val table       : (t list -> t) attribute_node
  val tr          : (t list -> t) attribute_node
  val td          : (?colspan:int -> ?rowspan:int -> t list -> t) attribute_node
  val select      : (?name:name -> t list -> t) attribute_node
  val option      :
    (?selected:unit -> ?value:string -> t list -> t) attribute_node
  val style       : (?type_:string -> t list -> t) attribute_node
  val class_def   : class_name -> style_attributes -> t
  val script      : string -> t
  val ul          : (t list -> t) attribute_node
  val li          : (t list -> t) attribute_node

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
    ?class_:string -> ?style:style_attributes ->
    string -> string list -> ?editable_infos:EditableInfos.t -> t list list -> t
end

module Make (Parameter : PARAMETER) :
  S with type name = Parameter.Name.t and type action = Parameter.Action.t

module MakeUnsafe (Parameter : UNSAFE_PARAMETER) :
  S with type name = Parameter.Name.t and type action = Parameter.Action.t
