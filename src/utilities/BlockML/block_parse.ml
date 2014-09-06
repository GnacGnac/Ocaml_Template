
open Result


type error = [
| `File_does_not_exist of string
| `Could_not_open_file of string
| `Unrecognized_char of char Position.t
| `Unterminated_comment of unit Position.t
| `Parse_error of unit Position.t
]


let from_file file =
  try
    if Sys.file_exists file then
      let lexbuf = Lexing.from_channel (open_in file) in
      Lexer_ext.set_file lexbuf file ;
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
