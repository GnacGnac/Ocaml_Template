
open Result


type attribute =
  | Color | Face | Type | Value | Border | Cellpadding | Cellspacing | Colspan
  | Rowspan | Text

type html_node =
  | Html | Body | Input | Font | Bold | Italic | Br | Paragraph | Table
  | Tr | Td | Center

type item = Attribute of attribute | Html_node of html_node

let html_Text = Text


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
     (Rowspan, "rowspan") ; (Text, "text")]
  let attribute_assoc_ =
    List.map (fun (x, y) -> (Attribute x, y)) attribute_assoc_
  let attribute_assoc = make_node_assoc attribute_assoc_
  let attribute_string_assoc = make_string_assoc attribute_assoc

  let html_node_assoc_ =
    [(Html, "html") ; (Body, "body") ; (Input, "input") ; (Font, "font") ;
     (Bold, "bold") ; (Italic, "italic") ; (Br, "br") ;
     (Paragraph, "paragraph") ; (Table, "table") ; (Tr, "tr") ; (Td, "td") ;
     (Center, "center")]
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

  let spec = function
    | _ ->
      Children.make
	BlockML.Occurrence.none BlockML.Occurrence.none Children.NodeMap.empty

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
let input ?typ ?text () =
  let typ = get_attribute Type typ in
  let text = get_attribute html_Text text in
  node Input (typ @ text)
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


let rec to_string space html = match Position.contents html with
  | Int i -> space ^ (string_of_int i)
  | Text s -> space ^ s
  | Node (name, children) -> to_string_node name children

and to_string_node name children =
  let is_attribute child = match Position.contents child with
    | Node (Attribute _, _) -> true
    | _ -> false in
  let (attributes, children) = List.partition is_attribute children in
  assert false (* TODO *)

let to_string = to_string ""
