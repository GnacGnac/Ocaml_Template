
let set_file_in_position position file =
  { position with Lexing.pos_fname = file }

let set_file lexbuf file =
  lexbuf.Lexing.lex_start_p <-
    set_file_in_position lexbuf.Lexing.lex_start_p file ;
  lexbuf.Lexing.lex_curr_p <-
    set_file_in_position lexbuf.Lexing.lex_curr_p file
