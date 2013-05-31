
open Result


let acute s = s ^ "acute"
let grave s = s ^ "grave"
let circ s = s ^ "circ"
let uml s = s ^ "uml"

let make_base_ints base has_bar =
  let base = int_of_char base.[1] in
  let rec aux i acc =
    if i > 3 then acc
    else aux (i + 1) (acc @ [base + i + (if has_bar & i = 3 then 1 else 0)]) in
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
    ([("&", "amp")] @
     (List.flatten
	(List.map string_voyel
	   [("a", "à", "À", true) ; ("e", "è", "È", false) ;
	    ("i", "ì", "Ì", false) ; ("o", "ò", "Ò", true) ;
	    ("u", "ù", "Ù", false)])))

let string s =
  let f res (to_replace, replacement) =
    Str.global_replace (Str.regexp to_replace) replacement res in
  List.fold_left f s special_characters

type attribute =
| Color | Face | Type | Value | Border | Cellpadding | Cellspacing | Colspan
| Rowspan | Action | Name | Method | Size | Bgcolor | Selected

let string_of_attribute = function
  | Color -> "color"
  | Face -> "face"
  | Type -> "type"
  | Value -> "value"
  | Border -> "border"
  | Cellpadding -> "cellpadding"
  | Cellspacing -> "cellspacing"
  | Colspan -> "colspan"
  | Rowspan -> "rowspan"
  | Action -> "action"
  | Name -> "name"
  | Method -> "method"
  | Size -> "size"
  | Bgcolor -> "bgcolor"
  | Selected -> "selected"

type node =
| Html | Body | Input | Font | Bold | Italic | Br | Paragraph | Table
| Tr | Td | Center | Form | Block | Select | Option | Strike

let string_of_node = function
  | Html -> "html"
  | Body -> "body"
  | Input -> "input"
  | Font -> "font"
  | Bold -> "b"
  | Italic -> "i"
  | Br -> "br"
  | Paragraph -> "paragraph"
  | Table -> "table"
  | Tr -> "tr"
  | Td -> "td"
  | Center -> "center"
  | Form -> "form"
  | Block -> "block"
  | Select -> "select"
  | Option -> "option"
  | Strike -> "strike"


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

  val to_string : t -> string

  module EditableInfos : sig
    type t
    val make :
      html list -> string -> name -> string -> name ->
      (string * name) list -> t
  end

  val result_table :
    ?border:int -> ?cellpadding:int -> ?cellspacing:int ->
    action -> string -> (int -> name) -> method_ -> string list ->
    ?editable_infos:EditableInfos.t -> t list list -> t
end


module Make (Parameter : PARAMETER) = struct

  type name = Parameter.Name.t
  type action = Parameter.Action.t

  type method_ = Get | Post
  let string_of_method_ = function
    | Get -> "GET"
    | Post -> "POST"

  type type_ = Text | Password | Text_area | Submit | Checkbox
  let string_of_type_ = function
    | Text -> "text"
    | Password -> "password"
    | Text_area -> "textarea"
    | Submit -> "submit"
    | Checkbox -> "checkbox"

  type color = Rgb of int * int * int
  let string_of_color = function
    | Rgb (red, green, blue) ->
      let hex_digit = function
	| i when i < 10 -> string_of_int i
	| i -> String.make 1 (char_of_int ((int_of_char 'A') - 10 + i)) in
      let to_hex i = (hex_digit ((i/16) mod 16)) ^ (hex_digit (i mod 16)) in
      "#" ^ (to_hex red) ^ (to_hex green) ^ (to_hex blue)

  type face = Arial
  let string_of_face = function
    | Arial -> "arial"

  type selected = Selected_value
  let string_of_selected = function
    | Selected_value -> "selected"

  type t =
  | Text of string
  | Node of node * (attribute * string) list * t list
  type html = t

  let node node attributes children = Node (node, attributes, children)

  let get_generic_attribute f attribute = function
    | None -> []
    | Some value -> [(attribute, f value)]

  let get_attribute = get_generic_attribute (fun s -> s)
  let get_int_attribute = get_generic_attribute string_of_int
  let get_name_attribute = get_generic_attribute Parameter.Name.to_string
  let get_method_attribute = get_generic_attribute string_of_method_
  let get_type_attribute = get_generic_attribute string_of_type_
  let get_color_attribute = get_generic_attribute string_of_color
  let get_face_attribute = get_generic_attribute string_of_face
  let get_selected_attribute = get_generic_attribute string_of_selected
  let get_action_attribute = get_generic_attribute Parameter.Action.to_string

  let string = string

  let text s = Text s
  let text_string s = text (string s)
  let html = node Html []
  let body = node Body []
  let input ?type_ ?value ?name ?size () =
    let type_ = get_type_attribute Type type_ in
    let value = get_attribute Value value in
    let name = get_name_attribute Name name in
    let size = get_int_attribute Size size in
    node Input (type_ @ value @ name @ size) []
  let font ?color ?face =
    let color = get_color_attribute Color color in
    let face = get_face_attribute Face face in
    node Font (color @ face)
  let bold = node Bold []
  let italic = node Italic []
  let paragraph = node Paragraph []
  let center = node Center []
  let br = node Br [] []
  let table ?border ?cellpadding ?cellspacing =
    let border = get_int_attribute Border border in
    let cellpadding = get_int_attribute Cellpadding cellpadding in
    let cellspacing = get_int_attribute Cellspacing cellspacing in
    node Table (border @ cellpadding @ cellspacing)
  let tr ?bgcolor =
    let bgcolor = get_color_attribute Bgcolor bgcolor in
    node Tr bgcolor
  let td ?colspan ?rowspan =
    let colspan = get_int_attribute Colspan colspan in
    let rowspan = get_int_attribute Rowspan rowspan in
    node Td (colspan @ rowspan)
  let form ?method_ ?action =
    let method_ = get_method_attribute Method method_ in
    let action = get_action_attribute Action action in
    node Form (method_ @ action)
  let spaces n = text (String_ext.repeat n "&nbsp;")
  let space = spaces 1
  let block = node Block []
  let select ?name =
    let name = get_name_attribute Name name in
    node Select name
  let option ?selected ?value =
    let selected = get_selected_attribute Selected selected in
    let value = get_attribute Value value in
    node Option (selected @ value)
  let strike = node Strike []


  let string_of_attribute_value attribute value =
    (string_of_attribute attribute) ^ "=\"" ^ value ^ "\""
  let string_of_attributes =
    let f res (attribute, value) =
      res ^ " " ^ (string_of_attribute_value attribute value) in
    List.fold_left f ""

  let rec to_string space = function
    | Text s -> space ^ s
    | Node (Block, [], children) -> to_string_children space children
    | Node (node, attributes, children) ->
      to_string_node space node attributes children

  and to_string_node space node attributes children =
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

  and to_string_children space children =
    List_ext.to_string "\n" (to_string space) children

  let to_string = to_string ""


  module EditableInfos = struct

    type t =
      { line_add_cells : html list ;
	add_value : string ;
	add_button : name ;
	action_value : string ;
	action_button : name ;
	actions : (string * name) list }

    let make
	line_add_cells add_value add_button action_value action_button actions =
      { line_add_cells ; add_value ; add_button ; action_value ; action_button ;
	actions}

    let line_add_cells infos = infos.line_add_cells
    let add_value infos = infos.add_value
    let add_button infos = infos.add_button
    let action_value infos = infos.action_value
    let action_button infos = infos.action_button
    let actions infos = infos.actions

  end

  let result_table
      ?border ?cellpadding ?cellspacing
      destination name cell_id method_ line_names ?editable_infos contents =
    let td ?colspan contents = td ?colspan [center contents] in
    let td_one ?colspan cell = td ?colspan [cell] in
    let editable = editable_infos <> None in
    let add_value = Option.map EditableInfos.add_value editable_infos in
    let name_add = Option.map EditableInfos.add_button editable_infos in
    let f_contents index tr_contents =
      let tr_contents =
	let name = cell_id index in
	let needs_checkbox = match editable_infos with
	  | None -> false
	  | Some infos -> EditableInfos.actions infos <> [] in
	let checkbox =
	  if editable then
	    if needs_checkbox then [input ~type_:Checkbox ~name ()]
	    else [space]
	  else [] in
	tr_contents @ checkbox in
      let tr_contents = List.map td_one tr_contents in
      let bgcolor =
	if index mod 2 = 0 then None else Some (Rgb (0xD3, 0xD3, 0xD3)) in
      tr ?bgcolor tr_contents in
    let contents = List_ext.mapi f_contents contents in
    let cell_number = List.length line_names in
    let line_names = List.map text line_names in
    let line_names =
      List.map td_one (line_names @ (if editable then [space] else [])) in
    let line_add = match editable_infos with
      | None -> []
      | Some infos ->
	(EditableInfos.line_add_cells infos) @
	  [input ~type_:Submit ?name:name_add ?value:add_value ()] in
    let line_add =
      if editable then
	[tr ~bgcolor:(Rgb (0xCE, 0xF6, 0xF5)) (List.map td_one line_add)]
      else [] in
    let line_edit = match editable_infos with
      | None -> []
      | Some infos ->
	let action_value = EditableInfos.action_value infos in
	let action = EditableInfos.action_button infos in
	let actions = EditableInfos.actions infos in
	let f_action i (text, name) =
	  let value = Parameter.Name.to_string name in
	  let selected = if i = 0 then Some Selected_value else None in
	  option ~value ?selected [text_string text] in
	let actions = List_ext.mapi f_action actions in
	if actions = [] then []
	else
	  [tr ~bgcolor:(Rgb (0xF5, 0xA9, 0xA9))
	      [td ~colspan:cell_number [space] ;
	       td
		 [select ~name:action actions ;
		  br ; br ;
		  input ~type_:Submit ~value:action_value ()]]] in
    form ~action:destination ~method_
      [table ?border ?cellpadding ?cellspacing
	  ([tr ~bgcolor:(Rgb (0xa9, 0xa9, 0xf5))
	       [td_one ~colspan:(cell_number + (if editable then 1 else 0))
		   (bold [text name])] ;
	    tr ~bgcolor:(Rgb (0xCE, 0xF6, 0xF5)) line_names] @
	      line_add @
	      contents @
	      line_edit)]

end


module MakeUnsafe (Parameter : UNSAFE_PARAMETER) = struct

  module SafeParameter = struct
    module Name = String_ext.MakeStringable (Parameter.Name)
    module Action = String_ext.MakeStringable (Parameter.Action)
  end

  include Make (SafeParameter)

end
