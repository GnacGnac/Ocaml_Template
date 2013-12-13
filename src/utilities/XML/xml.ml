
open Result
open Xml_parse


module Infos = struct

  type t = { pos : unit Position.t ; value : string Position.t }

  let pos infos = infos.pos
  let value infos = infos.value

  let make pos value = { pos ; value }

  let compare = Pervasives.compare

end


module Attributes = struct

  module Map = Map_ext.Make1 (String) (Infos)

  type t = Map.t

  let empty = Map.empty

  let get_pos attributes attribute =
    map_result Infos.pos (Map.find attribute attributes)

  let extract_pos attributes = get_pos attributes |> extract

  let get_with_pos attributes attribute =
    map_result Infos.value (Map.find attribute attributes)

  let get attributes attribute =
    map_result Position.contents (get_with_pos attributes attribute)

  let extract_with_pos attributes = get_with_pos attributes |> extract

  let extract attributes = extract_with_pos attributes |> Position.contents

  let update_with_pos attributes attribute value =
    let pos = Position.map_contents (fun _ -> ()) attribute in
    let key = Position.contents attribute in
    let infos = Infos.make pos value in
    Map.add key infos attributes

  let update attributes attribute value =
    let old_value = get_with_pos attributes attribute in
    let old_value_changed =
      map_result (Position.change_contents value) old_value in
    let value = Result.get (Position.make_dummy value) old_value_changed in
    let pos =
      Result.get (Position.make_dummy ()) (get_pos attributes attribute) in
    let attribute = Position.change_contents attribute pos in
    update_with_pos attributes attribute value

  let add_with_pos attributes attribute value =
    let key = Position.contents attribute in
    if Map.exists (fun key' _ -> key' = key) attributes then
      error `Attribute_already_exists
    else return (update_with_pos attributes attribute value)

  let add attributes attribute value =
    if Map.exists (fun key' _ -> key' = attribute) attributes then
      error `Attribute_already_exists
    else return (update attributes attribute value)

  let remove attributes attribute = Map.remove attribute attributes

  let fold_with_pos f =
    let f' key infos =
      let key = Position.change_contents key (Infos.pos infos) in
      let value = Infos.value infos in
      f key value in
    Map.fold f'

  let fold f =
    let f' key value =
      let key = Position.contents key in
      let value = Position.contents value in
      f key value in
    fold_with_pos f'

end


type t =
  { node : string Position.t ;
    attributes : Attributes.t ;
    children : t list }

let node_with_pos node attributes children =  { node ; attributes ; children }
let node node = node_with_pos (Position.make_dummy node)

let get_node_with_pos xml = xml.node
let get_node = get_node_with_pos |> Position.contents

let get_attributes xml = xml.attributes
let get_attribute xml = Attributes.get (get_attributes xml)
let get_attribute_with_pos xml = Attributes.get_with_pos (get_attributes xml)
let extract_attribute xml = get_attribute xml |> extract
let extract_attribute_with_pos xml = get_attribute_with_pos xml |> extract

let get_children xml = xml.children

let get_node_children xml node =
  List.filter (get_node |> (=) node) (get_children xml)

let get_node_child xml node = match get_node_children xml node with
  | [] -> error `Not_found
  | node :: _ -> return node

let extract_node_child xml = get_node_child xml |> extract


let check_tags start_tag end_tag =
  if Position.contents start_tag = Position.contents end_tag then return ()
  else error (`Different_opening_and_closing_tags (start_tag, end_tag))

let from_attributes xml_parsing_attributes =
  assert false (* TODO *)

let rec from_xml_parsing xml_parsing =
  let start_tag = Xml_parsing.start_tag xml_parsing in
  let end_tag = Xml_parsing.start_tag xml_parsing in
  check_tags start_tag end_tag >>= fun () ->
  from_attributes (Xml_parsing.attributes xml_parsing) >>= fun attributes ->
  List_ext.bind from_xml_parsing (Xml_parsing.children xml_parsing) >>=
    fun children ->
  return (node_with_pos start_tag attributes children)

let from_file file = Xml_parse.from_file file >>= from_xml_parsing
