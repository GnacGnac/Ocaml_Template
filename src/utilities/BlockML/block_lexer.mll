{

  open Block_parser

  exception Unrecognized_char of char

  let pos_of_lexbuf lexbuf =
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      (line, char)

  let position_from_buffer lexbuf a =
    let (line, char) = pos_of_lexbuf lexbuf in
    Position.make a line char

  module Chars : sig
    val reset : Lexing.lexbuf -> unit
    val add : char -> unit
    val get : unit -> string * int * int
  end = struct
    let chars : string ref = ref ""
    let line = ref 1
    let char = ref 0
    let reset lexbuf =
      let (ln, ch) = pos_of_lexbuf lexbuf in
      chars := "" ;
      line := ln ;
      char := ch
    let add c = chars := !chars ^ (String.make 1 c)
    let get () = (!chars, !line, !char)
  end

  module CommentLevel : sig
    val increase : unit -> unit
    val decrease : unit -> unit
    val is_null : unit -> bool
  end = struct
    let level = ref 0
    let increase () = level := !level + 1
    let decrease () = level := !level - 1
    let is_null () = !level = 0
  end

}


let space    = [' ' '\t']
let new_line = ['\n']
let digit    = ['0'-'9']
let integer  = digit+
let letter   = ['a'-'z' 'A'-'Z']
let letter_  = letter | '_'
let ident    = letter letter_* digit*


rule token = parse
  | space        { token lexbuf }
  | new_line     { Lexing.new_line lexbuf ; token lexbuf }
  | '"'          { Chars.reset lexbuf ; quoted_string lexbuf }
  | '{'          { LBRC }
  | '}'          { RBRC }
  | "/*"         { comment lexbuf }
  | integer as i { INT (position_from_buffer lexbuf (int_of_string i)) }
  | ident as s   { IDENT (position_from_buffer lexbuf s) }
  | eof          { EOF }
  | _ as c       { raise (Unrecognized_char c) }

and quoted_string = parse
  | new_line as c { Lexing.new_line lexbuf ; Chars.add c ;
		    quoted_string lexbuf }
  | "\\\""        { Chars.add '"' ; quoted_string lexbuf }
  | "\\\\"        { Chars.add '\\' ; quoted_string lexbuf }
  | '"'           { let (s, line, char) = Chars.get () in
		    STRING (Position.make s line char) }
  | _ as c        { Chars.add c ; quoted_string lexbuf }

and comment = parse
  | "*/" { CommentLevel.decrease () ;
	   if CommentLevel.is_null () then token lexbuf
	   else comment lexbuf }
  | "/*" { CommentLevel.increase () ; comment lexbuf }
  | _    { comment lexbuf }


{

}
