
open Result
open BlockML.Generic


let string_of_error = function
  | `File_does_not_exist file -> "file " ^ file ^ " does not exist"
  | `Could_not_open_file file -> "could not open file " ^ file
  | `Unrecognized_char (c, pos) ->
    Printf.sprintf "line %d character %d, unrecognized character '%c'"
      (Position.line pos) (Position.char pos) c
  | `Parse_error pos ->
    Printf.sprintf "parse error line %d character %d"
      (Position.line pos) (Position.char pos)
  | `Node_not_bound_to_a_string node ->
    "node " ^ node ^ " not bound to a string"


let _ = match Parse.from_file Sys.argv.(1) >>= Block_string.to_string with
  | Ok s -> Printf.printf "%s\n%!" s
  | Error error -> Error.show (string_of_error error)

