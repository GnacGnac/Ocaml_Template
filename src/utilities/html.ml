
open Result


type node =
  | Html | Body | Input | Font | Bold | Italic | Br | Paragraph | Table
  | Tr | Td | Center
  | Color | Face | Type | Value | Border | Cellpadding | Cellspacing | Colspan
  | Rowspan | Text

let html_Text = Text


module M = struct

  module Ord = struct
    type t = node
    let compare = Pervasives.compare
  end

  include Ord

  module Set = Set_ext.Make (Ord)

  let node_assoc_ =
    [(Html, "html") ; (Body, "body") ; (Input, "input") ; (Font, "font") ;
     (Bold, "bold") ; (Italic, "italic") ; (Br, "br") ;
     (Paragraph, "paragraph") ; (Table, "table") ; (Tr, "tr") ; (Td, "td") ;
     (Center, "center") ;
     (Type, "type") ; (Text, "text")]
  let node_assoc = List.map (fun (x, y) -> (x, String.lowercase y)) node_assoc_
  let string_assoc = List.map (fun (x, y) -> (y, x)) node_assoc

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

  let possible_roots = Set.singleton Html

end

module Instance = BlockML.Instance.Make (M)
include Instance


let html = node Html
let body = node Body
let input ?typ ?text () =
  let typ = match typ with
    | None -> []
    | Some typ -> [node Type [Instance.text typ]] in
  let text = match text with
    | None -> []
    | Some text -> [node html_Text [Instance.text text]] in
  node Input (typ @ text)
(*
val font      : t list -> t
val bold      : t list -> t
val italic    : t list -> t
val paragraph : t list -> t
val table     :
  border:int -> ?cellpadding:int -> ?cellspacing:int -> t list -> t
val tr        : make_node
val td        : ?colspan:int -> ?rowspan:int -> t list -> t
val center    : t list -> t
val br        : t
*)
