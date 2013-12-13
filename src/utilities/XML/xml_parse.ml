
open Result


let from_file file =
  try
    if Sys.file_exists file then
      let lexbuf = Lexing.from_channel (open_in file) in
      Lexer_ext.set_file lexbuf file ;
      try return (Xml_parser.root Xml_lexer.token lexbuf)
      with
      | Xml_lexer.Unrecognized_char c -> error (`Unrecognized_char c)
      | Xml_lexer.Unterminated_comment pos -> error (`Unterminated_comment pos)
      | Parsing.Parse_error ->
	error (`Parse_error (Xml_lexer.position_from_buffer lexbuf ()))
    else error (`File_does_not_exist file)
  with
  | Sys_error _ -> error (`Could_not_open_file file)
