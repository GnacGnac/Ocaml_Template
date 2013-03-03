
open Result


type attribute =
  | Color | Face | Type | Value | Border | Cellpadding | Cellspacing | Colspan
  | Rowspan | Action | Name | Method | Size

type html_node =
  | Html | Body | Input | Font | Bold | Italic | Br | Paragraph | Table
  | Tr | Td | Center | Form

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
     (Method, "method") ; (Size, "size")]
  let attribute_assoc_ =
    List.map (fun (x, y) -> (Attribute x, y)) attribute_assoc_
  let attribute_assoc = make_node_assoc attribute_assoc_
  let attribute_string_assoc = make_string_assoc attribute_assoc

  let html_node_assoc_ =
    [(Html, "html") ; (Body, "body") ; (Input, "input") ; (Font, "font") ;
     (Bold, "bold") ; (Italic, "italic") ; (Br, "br") ;
     (Paragraph, "paragraph") ; (Table, "table") ; (Tr, "tr") ; (Td, "td") ;
     (Center, "center") ; (Form, "form")]
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

  let node_spec int_children text_children node_children attribute_children =
    let f_node (node, occurrences) = (Html_node node, occurrences) in
    let f_attribute attribute =
      (Attribute attribute, BlockML.Occurrence.option) in
    Children.make int_children text_children
      (Children.NodeMap.of_list
	 ((List.map f_node node_children) @
	  (List.map f_attribute attribute_children)))

  let html_spec =
    node_spec BlockML.Occurrence.none BlockML.Occurrence.none
      [(Body, BlockML.Occurrence.option)] []

  let body_node_children =
    List.map (fun child -> (child, BlockML.Occurrence.any))
      [Input ; Font ; Bold ; Italic ; Br ; Paragraph ; Table ; Tr ; Td ;
       Center ; Form]

  let body_spec =
    node_spec BlockML.Occurrence.any BlockML.Occurrence.any
      body_node_children []

  let input_spec =
    node_spec BlockML.Occurrence.any BlockML.Occurrence.any
      [] [Type ; Value ; Name ; Size]

  let font_spec =
    node_spec BlockML.Occurrence.any BlockML.Occurrence.any
      body_node_children [Color ; Face]

  let bold_spec = body_spec

  let italic_spec = body_spec

  let br_spec = node_spec BlockML.Occurrence.none BlockML.Occurrence.none [] []

  let paragraph_spec = body_spec

  let table_spec =
    node_spec BlockML.Occurrence.none BlockML.Occurrence.none
      [(Tr, BlockML.Occurrence.any)]
      [Border ; Cellpadding ; Cellspacing]

  let tr_spec =
    node_spec BlockML.Occurrence.none BlockML.Occurrence.none
      [(Td, BlockML.Occurrence.any)] []

  let td_spec = body_spec

  let center_spec = body_spec

  let form_spec =
    node_spec BlockML.Occurrence.any BlockML.Occurrence.any
      body_node_children [Method ; Action]

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

  let int_attribute =
    Children.make
      BlockML.Occurrence.one BlockML.Occurrence.none Children.NodeMap.empty

  let string_attribute =
    Children.make
      BlockML.Occurrence.none BlockML.Occurrence.one Children.NodeMap.empty

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


  let spec = function
    | Html_node node -> node_spec node
    | Attribute attribute -> attribute_spec attribute

  let possible_roots = Set.singleton (Html_node Html)

end

module Instance = BlockML.Instance.Make (M)
include Instance


let get_generic_attribute f attribute = function
  | None -> []
  | Some value -> [node (Attribute attribute) [f value]]

let get_attribute = get_generic_attribute Instance.text
let get_int_attribute = get_generic_attribute Instance.int

let node html_node = node (Html_node html_node)

let html = node Html
let body = node Body
let input ?typ ?value ?name ?size () =
  let typ = get_attribute Type typ in
  let value = get_attribute Value value in
  let name = get_attribute Name name in
  let size = get_int_attribute Size size in
  node Input (typ @ value @ name @ size)
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
let tr = node Tr
let td ?colspan ?rowspan children =
  let colspan = get_int_attribute Colspan colspan in
  let rowspan = get_int_attribute Rowspan rowspan in
  node Td (colspan @ rowspan @ children)
let form ?method_ ?action children =
  let method_ = get_attribute Method method_ in
  let action = get_attribute Action action in
  node Form (method_ @ action @ children)


let string_of_attribute attribute value =
  (Node.to_string attribute) ^ "=\"" ^ value ^ "\""
let string_of_attribute attribute = match Position.contents attribute with
  | Node (attribute, [value]) ->
    (match Position.contents value with
      | Int value -> string_of_attribute attribute (string_of_int value)
      | Text value -> string_of_attribute attribute value
      | _ -> assert false (* should be impossible *))
  | _ -> assert false (* should be impossible *)

let string_of_attributes attributes =
  let f res attribute = res ^ " " ^ (string_of_attribute attribute) in
  List.fold_left f "" attributes

let rec to_string space html = match Position.contents html with
  | Int i -> space ^ (string_of_int i)
  | Text s -> space ^ s
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
