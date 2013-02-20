
open Result


module S = struct

  module M = struct
    type t = Html | Body
    let compare = Pervasives.compare
    let to_string = function
      | Html -> "html"
      | Body -> "body"
    let of_string = function
      | s when String.lowercase s = "html" -> return Html
      | s when String.lowercase s = "body" -> return Body
      | s -> error (`Unrecognized_string s)
  end
  include M
  module Set = Set_ext.Make (M)

  module Children = BlockML.ChildrenSpec.Make (M)

  let spec = function
    | M.Html ->
      Children.make
	BlockML.Occurrence.none
	BlockML.Occurrence.none
	(Children.NodeMap.of_list [(M.Body, BlockML.Occurrence.exactly 1)])
    | M.Body ->
      Children.make
	BlockML.Occurrence.none
	BlockML.Occurrence.any
	Children.NodeMap.empty

  let possible_roots = Set.singleton M.Html

end

module BlockString = BlockML.Instance.Make (S)


let string_of_error = function
  | `Bad_int_occurrence (node, occurrence, possible_occurrences) ->
    Printf.sprintf "bad int children occurrence of node %s (%d not in %s)"
      (BlockString.Node.to_string node)
      occurrence (BlockML.Occurrence.to_string possible_occurrences)
  | `Bad_text_occurrence (node, occurrence, possible_occurrences) ->
    Printf.sprintf "bad text children occurrence (%d not in %s)"
      occurrence (BlockML.Occurrence.to_string possible_occurrences)
  | `Bad_sub_node_occurrence
      (node, sub_node, occurrence, possible_occurrences) ->
    Printf.sprintf "bad text children occurrence (%d not in %s)"
      occurrence (BlockML.Occurrence.to_string possible_occurrences)
  | `File_does_not_exist file -> "file " ^ file ^ " does not exist"
  | `Could_not_open_file file -> "could not open file " ^ file
  | `Unrecognized_char (c, pos) ->
    Printf.sprintf "line %d character %d, unrecognized character '%c'"
      (Position.line pos) (Position.char pos) c
  | `Parse_error pos ->
    Printf.sprintf "parse error line %d character %d"
      (Position.line pos) (Position.char pos)
  | `Not_a_root_node None -> "root is not a node"
  | `Not_a_root_node (Some node) -> "node is not a root node"
  | `Unrecognized_node node -> "unrecognized node"


let _ =
  match
    BlockString.parse Sys.argv.(1) >>= fun block ->
    return (BlockString.to_string block) with
  | Ok s -> Printf.printf "%s\n%!" s
  | Error error -> Error.show (string_of_error error)

