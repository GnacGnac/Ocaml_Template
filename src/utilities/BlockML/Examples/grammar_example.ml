
open Result


let string_of_pos a = match Position.line_and_char a with
  | Ok (line, char) -> Printf.sprintf "line %d, char %d, " line char
  | Error `No_position -> ""

let string_of_error f = function
  | `Parse_error pos -> (string_of_pos pos) ^ " parse_error"
  | `Bad_sub_node_occurrence (parent, child, i, occ) ->
    Printf.sprintf "%sbad child occurrence %s for %s (%d not in %s)"
      (string_of_pos parent) (f child) (f (Position.contents parent)) i
      (BlockML.Occurrence.to_string occ)
  | `Not_a_root_node node_opt ->
    Printf.sprintf "%s%s is not a root node"
      (string_of_pos node_opt)
      (match Position.contents node_opt with
	| None -> "primitive node"
	| Some node -> f node)
  | `Unrecognized_char c ->
    Printf.sprintf "%sunrecognized character `%c`"
      (string_of_pos c) (Position.contents c)
  | `Unterminated_comment pos ->
    Printf.sprintf "%sunterminated comment" (string_of_pos pos)
  | `File_does_not_exist file -> "file " ^ file ^ " does not exist"
  | `Could_not_open_file file -> "could not open file " ^ file
  | `Bad_int_occurrence (node, i, occ) ->
    Printf.sprintf "%sbad int children occurrence for %s (%d not in %s)"
      (string_of_pos node) (f (Position.contents node)) i
      (BlockML.Occurrence.to_string occ)
  | `Bad_text_occurrence (node, i, occ) ->
    Printf.sprintf "%sbad text children occurrence for %s (%d not in %s)"
      (string_of_pos node) (f (Position.contents node)) i
      (BlockML.Occurrence.to_string occ)
  | `Unrecognized_node s ->
    Printf.sprintf "%sunrecognized node %s"
      (string_of_pos s) (Position.contents s)

let show_error f error = Error.show (string_of_error f error)

let run () =
  if Array.length Sys.argv >= 3 then
    match BlockML.Grammar.from_file Sys.argv.(1) with
      | Ok grammar ->
	let module Grammar = (val grammar : BlockML.Instance.S) in
	(match Grammar.parse Sys.argv.(2) with
	  | Ok block -> Printf.printf "%s\n%!" (Grammar.to_string block)
	  | Error error -> show_error Grammar.Node.to_string error)
      | Error error -> show_error BlockML.Grammar.Node.to_string error
  else Error.show ("usage: " ^ Sys.argv.(0) ^ " grammar_file file")
