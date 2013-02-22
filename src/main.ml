
open Result


(*
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
  | `Unrecognized_char pos ->
    let f_pos line char _ = Printf.sprintf "line %d character %d, " line char in
    let f_no_pos _ = "" in
    let prefix = Position.apply f_pos f_no_pos pos in
    Printf.sprintf "%sunrecognized character '%c'"
      prefix (Position.contents pos)
  | `Parse_error pos ->
    let f_pos line char _ = Printf.sprintf " %d character %d" line char in
    let f_no_pos _ = "" in
    let suffix = Position.apply f_pos f_no_pos pos in
    Printf.sprintf "parse error line%s" suffix
  | `Not_a_root_node None -> "root is not a node"
  | `Not_a_root_node (Some node) -> "node is not a root node"
  | `Unrecognized_node node -> "unrecognized node"
*)


let _ =
  let html =
    Html.html
      [Html.body
	  [Html.text "Hello world!" ;
	   Html.input ~typ:"hidden" ()]] in
  Printf.printf "%s\n%!" (Html.to_string html)
