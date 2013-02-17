
open Result


let from_file file =
  try
    if Sys.file_exists file then
      let lexbuf = Lexing.from_channel (open_in file) in
      try return (Parser.root Lexer.token lexbuf)
      with
	| Lexer.Unrecognized_char c ->
	  error (`Unrecognized_char (c, Position.of_buffer lexbuf))
	| Parsing.Parse_error ->
	  error (`Parse_error (Position.of_buffer lexbuf))
    else error (`File_does_not_exist file)
  with
    | Sys_error _ -> error (`Could_not_open_file file)
