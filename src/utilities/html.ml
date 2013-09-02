
open Result


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
  val green : color
  val black : color
  val white : color
  val transparent : color
  type face = Arial
  type font_family = Face of face
  type alignment = Left | Right | Center | Top | Down | Justify
  type size =
  | Percent of int | Pixel of int | Absolute of int | Auto | Em_size of int
  type cursor = Pointer
  type position = Absolute_position
  type list_style_type = No_list_style_type
  type consistence = Solid
  type border_attribute =
  | Border_size of size
  | Border_consistence of consistence
  | Border_color of color
  type border_attributes = border_attribute list
  type class_name =
  | Class_node of string
  | Sub_class of string * string
  | New_class of string
  type style_attribute =
  | Text_align of alignment
  | Color of color
  | Background_color of color
  | Cursor of cursor
  | Border of border_attributes
  | Margin of size
  | Padding of size
  | Font_size of size
  | Font_family of font_family list
  | Position of position
  | List_style_type of list_style_type
  | Alignment of (alignment * size)
  | Padding_left of size
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
  val a           : (?href:action -> t list -> t) attribute_node
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


module Make (Parameter : PARAMETER) = struct

  type name = Parameter.Name.t
  type action = Parameter.Action.t

  type method_ = Get | Post
  let string_of_method_ = function
    | Get -> "GET"
    | Post -> "POST"

  type type_ = Text | Password | Text_area | Submit | Checkbox | Hidden
  let string_of_type_ = function
    | Text -> "text"
    | Password -> "password"
    | Text_area -> "textarea"
    | Submit -> "submit"
    | Checkbox -> "checkbox"
    | Hidden -> "hidden"

  type color = Rgb of int * int * int | Transparent
  let string_of_color = function
    | Rgb (red, green, blue) ->
      "rgb(" ^ (string_of_int (red mod 256)) ^ "," ^
	(string_of_int (green mod 256)) ^ "," ^ (string_of_int (blue mod 256)) ^
	")"
    | Transparent -> "transparent"

  let red = Rgb (255, 0, 0)
  let blue = Rgb (0, 0, 255)
  let green = Rgb (0, 255, 0)
  let black = Rgb (0, 0, 0)
  let white = Rgb (255, 255, 255)
  let bg_main = Rgb (0xA9, 0xA9, 0xF5)
  let bg_title = Rgb (0xCE, 0xF6, 0xF5)
  let bg1 = white
  let bg2 = Rgb (0xD3, 0xD3, 0xD3)
  let bg_submit = Rgb (0xF5, 0xA9, 0xA9)
  let transparent = Transparent

  type face = Arial
  let string_of_face_plain = function
    | Arial -> "arial"
  let string_of_face face = "\"" ^ (string_of_face_plain face) ^ "\""

  type font_family = Face of face
  let string_of_font_family = function
    | Face face -> string_of_face face
  let string_of_font_family_list = List_ext.to_string "," string_of_font_family

  type alignment = Left | Right | Center | Top | Down | Justify
  let string_of_alignment = function
    | Left -> "left"
    | Right -> "right"
    | Center -> "center"
    | Top -> "top"
    | Down -> "down"
    | Justify -> "justify"

  type size =
  | Percent of int | Pixel of int | Absolute of int | Auto | Em_size of int
  let string_of_size = function
    | Percent i -> (string_of_int i) ^ "%"
    | Pixel i -> (string_of_int i) ^ "px"
    | Absolute i -> string_of_int i
    | Auto -> "auto"
    | Em_size i -> (string_of_int i) ^ "em"

  type cursor = Pointer
  let string_of_cursor = function
    | Pointer -> "pointer"

  type position = Absolute_position
  let string_of_position = function
    | Absolute_position -> "absolute"

  type list_style_type = No_list_style_type
  let string_of_list_style_type = function
    | No_list_style_type -> "none"

  type consistence = Solid
  let string_of_consistence = function
    | Solid -> "solid"

  type border_attribute =
  | Border_size of size
  | Border_consistence of consistence
  | Border_color of color
  let string_of_border_attribute = function
    | Border_size size -> string_of_size size
    | Border_consistence consistence -> string_of_consistence consistence
    | Border_color color -> string_of_color color

  type border_attributes = border_attribute list
  let string_of_border_attributes =
    List_ext.to_string " " string_of_border_attribute

  type class_name =
  | Class_node of string
  | Sub_class of string * string
  | New_class of string
  let string_of_class_name = function
    | Class_node s -> s
    | Sub_class (node, class_) -> node ^ "." ^ class_
    | New_class class_ -> "." ^ class_

  type style_attribute =
  | Text_align of alignment
  | Color of color
  | Background_color of color
  | Cursor of cursor
  | Border of border_attributes
  | Margin of size
  | Padding of size
  | Font_size of size
  | Font_family of font_family list
  | Position of position
  | List_style_type of list_style_type
  | Alignment of (alignment * size)
  | Padding_left of size

  let components_of_style_attribute = function
    | Text_align alignment -> ("text-align", string_of_alignment alignment)
    | Color color -> ("color", string_of_color color)
    | Background_color color -> ("background-color", string_of_color color)
    | Cursor cursor -> ("cursor", string_of_cursor cursor)
    | Border attributes -> ("border", string_of_border_attributes attributes)
    | Margin size -> ("margin", string_of_size size)
    | Padding size -> ("padding", string_of_size size)
    | Font_size size -> ("font-size", string_of_size size)
    | Font_family font_family_list ->
      ("font-family", string_of_font_family_list font_family_list)
    | Position position -> ("position", string_of_position position)
    | List_style_type list_style_type ->
      ("list-style-type", string_of_list_style_type list_style_type)
    | Alignment (alignment, size) ->
      (string_of_alignment alignment, string_of_size size)
    | Padding_left size -> ("padding-left", string_of_size size)

  let string_of_style_attribute attribute =
    let (attribute, value) = components_of_style_attribute attribute in
    attribute ^ ":" ^ value ^ ";"

  type style_attributes = style_attribute list
  let string_of_style_attributes attributes =
    let attributes = List.map string_of_style_attribute attributes in
    List.fold_left (^) "" attributes

  type attribute =
  | Type of type_
  | Value of string
  | Action of Parameter.Action.t
  | Name of Parameter.Name.t
  | Method of method_
  | Class of string
  | Style of style_attributes
  | Href of Parameter.Action.t
  | Selected
  | Colspan of int
  | Rowspan of int
  | Style_type of string
  | On_submit of string

  let components_of_attribute = function
    | Type type_ -> ("type", string_of_type_ type_)
    | Value value -> ("value", value)
    | Action action -> ("action", Parameter.Action.to_string action)
    | Name name -> ("name", Parameter.Name.to_string name)
    | Method method_ -> ("method", string_of_method_ method_)
    | Class s -> ("class", s)
    | Style style_attributes ->
      ("style", string_of_style_attributes style_attributes)
    | Href action -> ("href", Parameter.Action.to_string action)
    | Selected -> ("selected", "selected")
    | Colspan i -> ("colspan", string_of_int i)
    | Rowspan i -> ("rowspan", string_of_int i)
    | Style_type s -> ("type", s)
    | On_submit s -> ("onsubmit", s)

  type node =
  | Html
  | Head
  | Body
  | Title
  | Input
  | Br
  | Paragraph
  | Table
  | Tr
  | Td
  | Form
  | Select
  | Option
  | A
  | Span
  | Div
  | Strong
  | Em
  | Style_node
  | Class_def of class_name * style_attributes
  | Script
  | Ul
  | Li

  let string_of_node = function
    | Html -> "html"
    | Head -> "head"
    | Body -> "body"
    | Title -> "title"
    | Input -> "input"
    | Br -> "br"
    | Paragraph -> "paragraph"
    | Table -> "table"
    | Tr -> "tr"
    | Td -> "td"
    | Form -> "form"
    | Select -> "select"
    | Option -> "option"
    | A -> "a"
    | Span -> "span"
    | Div -> "div"
    | Strong -> "strong"
    | Em -> "em"
    | Style_node -> "style"
    | Class_def _ -> assert false (* do not use on this argument *)
    | Script -> "script"
    | Ul -> "ul"
    | Li -> "li"

  let acute s = s ^ "acute"
  let grave s = s ^ "grave"
  let circ s = s ^ "circ"
  let uml s = s ^ "uml"

  let make_base_ints base has_bar =
    let base = int_of_char base.[1] in
    let rec aux i acc =
      if i > 3 then acc
      else
	aux (i + 1) (acc @ [base + i + (if has_bar & i = 3 then 1 else 0)]) in
    aux 0 []

  let string_voyel_accents voyel base_accent has_bar =
    let f base_int modifier =
      let s = String.copy base_accent in
      s.[1] <- char_of_int base_int ;
      (s, modifier voyel) in
    let base_ints = make_base_ints base_accent has_bar in
    let modifiers = [grave ; acute ; circ ; uml] in
    List.map2 f base_ints modifiers

  let string_voyel (voyel, base_min, base_maj, has_bar) =
    (string_voyel_accents voyel base_min has_bar) @
      (string_voyel_accents (String.uppercase voyel) base_maj has_bar)

  let special_characters = (* TODO: complete *)
    List.map
      (fun (to_replace, replacement) -> (to_replace, "&" ^ replacement ^ ";"))
      ([("&", "amp") ; ("Ç", "Ccedil") ; ("ç", "ccedil")] @
	  (List.flatten
	     (List.map string_voyel
		[("a", "à", "À", true) ; ("e", "è", "È", false) ;
		 ("i", "ì", "Ì", false) ; ("o", "ò", "Ò", true) ;
		 ("u", "ù", "Ù", false)])))

  let string s =
    let f res (to_replace, replacement) =
      Str.global_replace (Str.regexp to_replace) replacement res in
    List.fold_left f s special_characters

  type t =
  | Text of string
  | Node of node * attribute list * t list
  type html = t

  let node node attributes ?class_ ?style children =
    let class_attribute = match class_ with
      | None -> []
      | Some s -> [Class s] in
    let style_attribute = match style with
      | None -> []
      | Some attributes -> [Style attributes] in
    Node (node, class_attribute @ style_attribute @ attributes, children)

  type 'a attribute_node = ?class_:string -> ?style:style_attributes -> 'a

  let singleton_list_of_option = function
    | None -> []
    | Some a -> [a]

  let get_generic_attribute f a = List.map f (singleton_list_of_option a)

  let get_type_attribute =
    get_generic_attribute (fun attribute -> Type attribute)
  let get_value_attribute =
    get_generic_attribute (fun attribute -> Value attribute)
  let get_name_attribute =
    get_generic_attribute (fun attribute -> Name attribute)
  let get_method_attribute =
    get_generic_attribute (fun attribute -> Method attribute)
  let get_action_attribute =
    get_generic_attribute (fun attribute -> Action attribute)
  let get_href_attribute =
    get_generic_attribute (fun attribute -> Href attribute)
  let get_selected_attribute = get_generic_attribute (fun () -> Selected)
  let get_colspan_attribute = get_generic_attribute (fun i -> Colspan i)
  let get_rowspan_attribute = get_generic_attribute (fun i -> Rowspan i)
  let get_style_type_attribute =
    get_generic_attribute (fun attribute -> Style_type attribute)
  let get_onsubmit_attribute = get_generic_attribute (fun s -> On_submit s)
  let get_href_attribute =
    get_generic_attribute (fun attribute -> Href attribute)

  let exact_text s = Text s
  let text s = exact_text (string s)
  let html = node Html []
  let head = node Head []
  let body = node Body []
  let title = node Title []
  let div = node Div []
  let span = node Span []
  let strong = node Strong []
  let em = node Em []
  let paragraph = node Paragraph []
  let br ?class_ ?style () = node Br [] ?class_ ?style []
  let spaces n = exact_text (String_ext.repeat n "&nbsp;")
  let space = spaces 1
  let form ?class_ ?style ?method_ ?action ?onsubmit ?name children =
    let method_ = get_method_attribute method_ in
    let action = get_action_attribute action in
    let onsubmit = get_onsubmit_attribute onsubmit in
    let name = get_name_attribute name in
    node Form (method_ @ action @ onsubmit @ name) ?class_ ?style children
  let input ?class_ ?style ?type_ ?value ?name () =
    let type_ = get_type_attribute type_ in
    let value = get_value_attribute value in
    let name = get_name_attribute name in
    node Input (type_ @ value @ name) ?class_ ?style []
  let a ?class_ ?style ?href children =
    let href = get_href_attribute href in
    node A href ?class_ ?style children
  let table = node Table []
  let tr = node Tr []
  let td ?class_ ?style ?colspan ?rowspan children =
    let colspan = get_colspan_attribute colspan in
    let rowspan = get_rowspan_attribute rowspan in
    node Td (colspan @ rowspan) ?class_ ?style children
  let select ?class_ ?style ?name children =
    let name = get_name_attribute name in
    node Select name ?class_ ?style children
  let option ?class_ ?style ?selected ?value children =
    let selected = get_selected_attribute selected in
    let value = get_value_attribute value in
    node Option (selected @ value) ?class_ ?style children
  let style ?class_ ?style ?type_ children =
    let type_ = get_style_type_attribute type_ in
    node Style_node type_ ?class_ ?style children
  let class_def class_name attributes =
    Node (Class_def (class_name, attributes), [], [])
  let script s = node Script [] [exact_text s]
  let ul = node Ul []
  let li = node Li []


  let string_of_style_attributes_with_space space attributes =
    let f attribute = space ^ (string_of_style_attribute attribute) ^ "\n" in
    let attributes = List.map f attributes in
    List.fold_left (^) "" attributes

  let string_of_attribute attribute =
    let (attribute, value) = components_of_attribute attribute in
    attribute ^ "=\"" ^ value ^ "\""
  let string_of_attributes attributes =
    let f attribute = " " ^ (string_of_attribute attribute) in
    let attributes = List.map f attributes in
    List.fold_left (^) "" attributes

  let to_string_class_def space class_name attributes =
    let name = string_of_class_name class_name in
    Printf.sprintf "%s%s {\n%s%s}"
      space name
      (string_of_style_attributes_with_space (space ^ "  ") attributes) space

  let rec to_string space = function
    | Text s -> space ^ s
    | Node (node, attributes, children) ->
      to_string_node space node attributes children

  and to_string_regular_node space node attributes children =
    let name = string_of_node node in
    match children with
    | [] ->
      Printf.sprintf "%s<%s%s/>" space name (string_of_attributes attributes)
    | _ ->
      Printf.sprintf "%s<%s%s>\n%s\n%s</%s>"
	space name
	(string_of_attributes attributes)
	(to_string_children (space ^ "  ") children)
	space name

  and to_string_node space node attributes children = match node with
    | Class_def (class_name, attributes) ->
      to_string_class_def space class_name attributes
    | _ -> to_string_regular_node space node attributes children

  and to_string_children space children =
    List_ext.to_string "\n" (to_string space) children

  let to_string = to_string ""


  module Button = struct

    type t = { label : string ; name : name }

    let make label name = { label ; name }

    let label button = button.label
    let name button = button.name

  end

  module EditableInfos = struct

    type t =
      { line_add_cells : html list ;
	add_button : Button.t ;
	edit_button : Button.t ;
	edit_selected : name ;
	edit_options : (string * name) list ;
	cell_id : int -> name }

    let make
	~line_add_cells ~add_button ~edit_button ~edit_selected ~edit_options
	~cell_id =
      { line_add_cells ; add_button ; edit_button ; edit_selected ;
	edit_options ; cell_id }

    let line_add_cells infos = infos.line_add_cells
    let add_button infos = infos.add_button
    let edit_button infos = infos.edit_button
    let edit_selected infos = infos.edit_selected
    let edit_options infos = infos.edit_options
    let cell_id infos = infos.cell_id

  end

  let button_infos f infos =
    let button = f infos in
    (Button.label button, Button.name button)

  let result_table ?class_ ?style name line_names ?editable_infos contents =
    let td ?class_ ?style ?colspan ?rowspan contents =
      let style = Text_align Center :: (singleton_list_of_option style) in
      td ?class_ ~style ?colspan ?rowspan contents in
    let td_one ?class_ ?style ?colspan ?rowspan cell =
      td ?class_ ?style ?colspan ?rowspan [cell] in
    let (editable, has_edit_option) = match editable_infos with
      | None -> (false, false)
      | Some infos -> (true, EditableInfos.edit_options infos <> []) in
    let f_contents index tr_contents =
      let tr_contents =
	tr_contents @
	  (match editable_infos with
	  | None -> []
	  | Some infos ->
	    let name = EditableInfos.cell_id infos index in
	    if has_edit_option then [input ~type_:Checkbox ~name ()]
	    else [space]) in
      let tr_contents = List.map td_one tr_contents in
      let bgcolor = if index mod 2 = 0 then bg1 else bg2 in
      tr ~style:[Background_color bgcolor] tr_contents in
    let contents = List_ext.mapi f_contents contents in
    let cell_number = List.length line_names in
    let line_names = List.map text line_names in
    let line_names =
      List.map td_one (line_names @ (if editable then [space] else [])) in
    let line_add = match editable_infos with
      | None -> []
      | Some infos ->
	let (value, name) = button_infos EditableInfos.add_button infos in
	let line =
	  (EditableInfos.line_add_cells infos) @
	    [input ~type_:Submit ~name ~value ()] in
	[tr ~style:[Background_color bg_title] (List.map td_one line)] in
    let contents = match editable_infos, contents with
      | None, _ -> contents
      | _, [] -> []
      | Some infos, _ ->
	let (value, name) = button_infos EditableInfos.edit_button infos in
	let selected_name = EditableInfos.edit_selected infos in
	let f_option i (s, name) =
	  let value = Parameter.Name.to_string name in
	  let selected = if i = 0 then Some () else None in
	  option ~value ?selected [text s] in
	let edit_options =
	  List_ext.mapi f_option (EditableInfos.edit_options infos) in
	if edit_options = [] then contents
	else
	  contents @
	    [tr ~style:[Background_color bg_submit]
		[td ~colspan:cell_number [space] ;
		 td
		   [select ~name:selected_name edit_options ;
		    br () ; br () ;
		    input ~type_:Submit ~name ~value ()]]] in
    table ?class_ ?style
      ([tr ~style:[Background_color bg_main]
	   [td_one ~colspan:(cell_number + (if editable then 1 else 0))
	       (strong [text name])] ;
	tr ~style:[Background_color bg_title] line_names] @
	  line_add @ contents)

end


module MakeUnsafe (Parameter : UNSAFE_PARAMETER) = struct

  module SafeParameter = struct
    module Name = String_ext.MakeStringable (Parameter.Name)
    module Action = String_ext.MakeStringable (Parameter.Action)
  end

  include Make (SafeParameter)

end
