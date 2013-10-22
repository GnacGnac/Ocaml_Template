
open Result


module M = struct
  type t = Term | Var | App | Abs
  let string_assoc =
    [(Term, "term") ; (Var, "var") ; (App, "app") ; (Abs, "abs")]
end

let string_of_env f env =
  let f (exp, i) =
    (BlockML.ChildrenSpec.string_of_exp f exp) ^ " = " ^ (string_of_int i) in
  let s = List_ext.to_string "\n" f env in
  if s = "" then "empty" else s

let string_of_spec_violation f name env spec =
  "node `" ^ (f name) ^ "` violates the part of its specification:\n" ^
    (BlockML.ChildrenSpec.to_string f spec) ^ "\n  in the environment:\n" ^
    (string_of_env f env)

let string_of_pos a = match Position.all a with
  | Ok (file, line, char) ->
    Printf.sprintf "in file %s, line %d, character %d, " file line char
  | Error `No_position -> ""

let string_of_error f = function
  | `Parse_error pos -> (string_of_pos pos) ^ " parse error."
  | `Not_a_root_node node_opt ->
    Printf.sprintf "%s%s is not a root node."
      (string_of_pos node_opt)
      (match Position.contents node_opt with
	| None -> "primitive node."
	| Some node -> f node)
  | `Unrecognized_char c ->
    Printf.sprintf "%sunrecognized character `%c`."
      (string_of_pos c) (Position.contents c)
  | `Unterminated_comment pos ->
    Printf.sprintf "%sunterminated comment." (string_of_pos pos)
  | `File_does_not_exist file -> "file " ^ file ^ " does not exist."
  | `Could_not_open_file file -> "could not open file " ^ file ^ "."
  | `Unrecognized_node s ->
    Printf.sprintf "%sunrecognized node %s."
      (string_of_pos s) (Position.contents s)
  | `Grammar_unrecognized_node s ->
    Printf.sprintf "%s%s is not a node of the grammar."
      (string_of_pos s) (Position.contents s)
  | `Children_spec_violation (name, env, spec) ->
    Printf.sprintf "%s%s." (string_of_pos name)
      (string_of_spec_violation f (Position.contents name) env spec)

let show_error f error = Error.show (string_of_error f error)

let run () =
  if Array.length Sys.argv >= 3 then
    let grammar_file = Sys.argv.(1) in
    let file = Sys.argv.(2) in
    let m = (module M : String_ext.UNSAFE_STRINGABLE) in
    match BlockML.parse_from_external m grammar_file file with
    | Ok lambda ->
      let module Lambda = (val lambda : BlockML.PARSE_RESULT) in
      (match Lambda.parse_result with
      | Ok block -> Printf.printf "%s\n%!" (Lambda.to_string block)
      | Error error -> show_error Lambda.Node.to_string error)
    | Error (`Grammar_error error) ->
      show_error BlockML.Grammar.Node.to_string error
  else Error.show ("usage: " ^ Sys.argv.(0) ^ " grammar_file file")
