
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

type html_node =
  | Html | Body | Input | Font | Bold | Italic | Br | Paragraph | Table
  | Tr | Td | Center | Form | Block | Select | Option

type item = Attribute of attribute | Html_node of html_node


module M = struct

  module Ord = struct
    type t = item
    let compare = Pervasives.compare
  end

  include Ord

  module Set = Set_ext.Make (Ord)

  let make_node_assoc l = List.map (fun (x, y) -> (x, String.lowercase y)) l
  let make_string_assoc l = List.map (fun (x, y) -> (y, x)) l

  let attribute_assoc_ =
    [(Color, "color") ; (Face, "face") ; (Type, "type") ; (Value, "value") ;
     (Border, "border") ; (Cellpadding, "cellpadding") ;
     (Cellspacing, "cellspacing") ; (Colspan, "colspan") ;
     (Rowspan, "rowspan") ; (Action, "action") ; (Name, "name") ;
     (Method, "method") ; (Size, "size") ; (Bgcolor, "bgcolor") ;
     (Selected, "selected")]
  let attribute_assoc_ =
    List.map (fun (x, y) -> (Attribute x, y)) attribute_assoc_
  let attribute_assoc = make_node_assoc attribute_assoc_
  let attribute_string_assoc = make_string_assoc attribute_assoc

  let html_node_assoc_ =
    [(Html, "html") ; (Body, "body") ; (Input, "input") ; (Font, "font") ;
     (Bold, "b") ; (Italic, "i") ; (Br, "br") ;
     (Paragraph, "paragraph") ; (Table, "table") ; (Tr, "tr") ; (Td, "td") ;
     (Center, "center") ; (Form, "form") ; (Block, "block") ;
     (Select, "select") ; (Option, "option")]
  let html_node_assoc_ =
    List.map (fun (x, y) -> (Html_node x, y)) html_node_assoc_
  let html_node_assoc = make_node_assoc html_node_assoc_
  let html_node_string_assoc = make_string_assoc html_node_assoc

  let node_assoc = attribute_assoc @ html_node_assoc
  let string_assoc = attribute_string_assoc @ html_node_string_assoc

  let to_string node =
    try List.assoc node node_assoc
    with Not_found ->
      Error.show "node not associated to a string. Please report" ;
      assert false

  let of_string s =
    try return (List.assoc (String.lowercase s) string_assoc)
    with Not_found -> error (`Unrecognized_string s)

  module Children = BlockML.ChildrenSpec.Make (Ord)
  module Occurrence = BlockML.Occurrence
  module Primitive = BlockML.Primitive

  let node_spec primitive_children node_children attribute_children =
    let f_node (node, occurrences) = (Html_node node, occurrences) in
    let f_attribute attribute =
      (Attribute attribute, Occurrence.option) in
    Children.make primitive_children
      (Children.NodeMap.of_list
	 ((List.map f_node node_children) @
	  (List.map f_attribute attribute_children)))

  let html_spec = node_spec Primitive.none [(Body, Occurrence.option)] []

  let body_node_children =
    List.map (fun child -> (child, Occurrence.any))
      [Input ; Font ; Bold ; Italic ; Br ; Paragraph ; Table ; Tr ; Td ;
       Center ; Form ; Block ; Select]

  let body_spec = node_spec Primitive.anys body_node_children []

  let input_spec = node_spec Primitive.anys [] [Type ; Value ; Name ; Size]

  let font_spec = node_spec Primitive.anys body_node_children [Color ; Face]

  let bold_spec = body_spec

  let italic_spec = body_spec

  let br_spec = node_spec Primitive.none [] []

  let paragraph_spec = body_spec

  let table_spec =
    node_spec Primitive.none
      [(Tr, Occurrence.any)] [Border ; Cellpadding ; Cellspacing]

  let tr_spec = node_spec Primitive.none [(Td, Occurrence.any)] [Bgcolor]

  let td_spec = body_spec

  let center_spec = body_spec

  let form_spec = node_spec Primitive.anys body_node_children [Method ; Action]

  let block_spec = body_spec

  let select_spec = node_spec Primitive.none [(Option, Occurrence.any)] []

  let option_spec = node_spec Primitive.one_text [] [Value ; Selected]

  let node_spec = function
    | Html -> html_spec
    | Body -> body_spec
    | Input -> input_spec
    | Font -> font_spec
    | Bold -> bold_spec
    | Italic -> italic_spec
    | Br -> br_spec
    | Paragraph -> paragraph_spec
    | Table -> table_spec
    | Tr -> tr_spec
    | Td -> td_spec
    | Center -> center_spec
    | Form -> form_spec
    | Block -> block_spec
    | Select -> select_spec
    | Option -> option_spec

  let int_attribute = Children.make Primitive.one_int Children.NodeMap.empty

  let string_attribute = Children.make Primitive.one_text Children.NodeMap.empty

  let attribute_spec = function
    | Color -> string_attribute
    | Face -> string_attribute
    | Type -> string_attribute
    | Value -> string_attribute
    | Border -> int_attribute
    | Cellpadding -> int_attribute
    | Cellspacing -> int_attribute
    | Colspan -> int_attribute
    | Rowspan -> int_attribute
    | Action -> string_attribute
    | Name -> string_attribute
    | Method -> string_attribute
    | Size -> int_attribute
    | Bgcolor -> string_attribute
    | Selected -> string_attribute


  let spec = function
    | Html_node node -> node_spec node
    | Attribute attribute -> attribute_spec attribute

  let possible_roots = Set.singleton (Html_node Html)

end

module Instance = BlockML.Instance.Make (M)
include Instance
type html = t


let get_generic_attribute f attribute = function
  | None -> []
  | Some value -> [node (Attribute attribute) [f value]]

let get_attribute = get_generic_attribute Instance.text
let get_int_attribute = get_generic_attribute Instance.int

let node html_node = node (Html_node html_node)

let text_string s = text (string s)
let html = node Html
let body = node Body
let input ?type_ ?value ?name ?size () =
  let type_ = get_attribute Type type_ in
  let value = get_attribute Value value in
  let name = get_attribute Name name in
  let size = get_int_attribute Size size in
  node Input (type_ @ value @ name @ size)
let font ?color ?face children =
  let color = get_attribute Color color in
  let face = get_attribute Face face in
  node Font (color @ face @ children)
let bold = node Bold
let italic = node Italic
let paragraph = node Paragraph
let center = node Center
let br = node Br []
let table ?border ?cellpadding ?cellspacing children =
  let border = get_int_attribute Border border in
  let cellpadding = get_int_attribute Cellpadding cellpadding in
  let cellspacing = get_int_attribute Cellspacing cellspacing in
  node Table (border @ cellpadding @ cellspacing @ children)
let tr ?bgcolor children =
  let bgcolor = get_attribute Bgcolor bgcolor in
  node Tr (bgcolor @ children)
let td ?colspan ?rowspan children =
  let colspan = get_int_attribute Colspan colspan in
  let rowspan = get_int_attribute Rowspan rowspan in
  node Td (colspan @ rowspan @ children)
let form ?method_ ?action children =
  let method_ = get_attribute Method method_ in
  let action = get_attribute Action action in
  node Form (method_ @ action @ children)
let spaces n = text (String_ext.repeat n "&nbsp;")
let space = spaces 1
let block = node Block
let select = node Select
let option ?selected ?value children =
  let selected = get_attribute Selected selected in
  let value = get_attribute Value value in
  node Option (selected @ value @ children)


let string_of_primitive = function
  | Int i -> string_of_int i
  | Text s -> s
let string_of_attribute attribute value =
  (Node.to_string attribute) ^ "=\"" ^ value ^ "\""
let string_of_attribute attribute = match Position.contents attribute with
  | Node (attribute, [value]) ->
    (match Position.contents value with
      | Primitive prim ->
	string_of_attribute attribute (string_of_primitive prim)
      | _ -> assert false (* should be impossible *))
  | _ -> assert false (* should be impossible *)

let string_of_attributes attributes =
  let f res attribute = res ^ " " ^ (string_of_attribute attribute) in
  List.fold_left f "" attributes

let rec to_string space html = match Position.contents html with
  | Primitive prim -> space ^ (string_of_primitive prim)
  | Node (Html_node Block, children) -> to_string_children space children
  | Node (name, children) -> to_string_node space name children

and to_string_node space name children =
  let name = Node.to_string name in
  let is_attribute child = match Position.contents child with
    | Node (Attribute _, _) -> true
    | _ -> false in
  let (attributes, children) = List.partition is_attribute children in
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
    { line_add_cells : Instance.t list ;
      add_value : string ;
      add_button : string ;
      action_value : string ;
      action_button : string ;
      actions : (string * string) list }

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
	  if needs_checkbox then [input ~type_:"checkbox" ~name ()]
	  else [space]
	else [] in
      tr_contents @ checkbox in
    let tr_contents = List.map td_one tr_contents in
    let bgcolor = if index mod 2 = 0 then None else Some "#D3D3D3" in
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
	[input ~type_:"submit" ?name:name_add ?value:add_value ()] in
  let line_add =
    if editable then [tr ~bgcolor:"#CEF6F5" (List.map td_one line_add)]
    else [] in
  let line_edit = match editable_infos with
    | None -> []
    | Some infos ->
      let action_value = EditableInfos.action_value infos in
      let action = EditableInfos.action_button infos in
      let actions = EditableInfos.actions infos in
      let f_action i (text, value) =
	let selected = if i = 0 then Some "selected" else None in
	option ~value ?selected [text_string text] in
      let actions = List_ext.mapi f_action actions in
      if actions = [] then []
      else
	[tr ~bgcolor:"#F5A9A9"
	    [td ~colspan:cell_number [space] ;
	     td
	       [select actions ;
		br ; br ;
		input ~type_:"submit" ~name:action ~value:action_value ()]]] in
  form ~action:destination ~method_
    [table ?border ?cellpadding ?cellspacing
	([tr ~bgcolor:"#A9A9F5"
	     [td_one ~colspan:(cell_number + (if editable then 1 else 0))
		 (bold [text name])] ;
	  tr ~bgcolor:"#CEF6F5" line_names] @
	    line_add @
	    contents @
	    line_edit)]
