
open Result


let from_file file =
  try
    if Sys.file_exists file then
      let lexbuf = Lexing.from_channel (open_in file) in
      try return (Block_parser.root Block_lexer.token lexbuf)
      with
	| Block_lexer.Unrecognized_char c ->
	  error (`Unrecognized_char (Position.of_buffer lexbuf c))
	| Parsing.Parse_error ->
	  error (`Parse_error (Position.of_buffer lexbuf ()))
    else error (`File_does_not_exist file)
  with
    | Sys_error _ -> error (`Could_not_open_file file)
