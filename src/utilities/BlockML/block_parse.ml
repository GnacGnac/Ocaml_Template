
open Result


let set_file_in_position position file =
  { position with Lexing.pos_fname = file }

let set_file lexbuf file =
  lexbuf.Lexing.lex_start_p <-
    set_file_in_position lexbuf.Lexing.lex_start_p file ;
  lexbuf.Lexing.lex_curr_p <-
    set_file_in_position lexbuf.Lexing.lex_curr_p file

let from_file file =
  try
    if Sys.file_exists file then
      let lexbuf = Lexing.from_channel (open_in file) in
      set_file lexbuf file ;
      try return (Block_parser.root Block_lexer.token lexbuf)
      with
	| Block_lexer.Unrecognized_char c -> error (`Unrecognized_char c)
	| Block_lexer.Unterminated_comment pos ->
	  error (`Unterminated_comment pos)
	| Parsing.Parse_error ->
	  error (`Parse_error (Block_lexer.position_from_buffer lexbuf ()))
    else error (`File_does_not_exist file)
  with
    | Sys_error _ -> error (`Could_not_open_file file)
