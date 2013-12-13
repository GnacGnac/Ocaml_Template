
type attribute = string Position.t * string Position.t
type attributes = attribute list
type t =
  { start_tag : string Position.t ;
    end_tag : string Position.t ;
    attributes : attributes ;
    children : t list }

let make start_tag end_tag attributes children =
  { start_tag ; end_tag ; attributes ; children }

let start_tag xml = xml.start_tag
let end_tag xml = xml.end_tag
let attributes xml = xml.attributes
let children xml = xml.children
