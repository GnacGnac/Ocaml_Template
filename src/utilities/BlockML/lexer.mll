{

  open Parser

  exception Unrecognized_char of char

  module Chars : sig
    val reset : unit -> unit
    val add : char -> unit
    val get : unit -> string
  end = struct
    let chars : string ref = ref ""
    let reset () = chars := ""
    let add c = chars := !chars ^ (String.make 1 c)
    let get () = !chars
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
let letter   = ['a'-'z' 'A'-'Z']
let letter_  = letter | '_'
let ident    = letter letter_* digit*


rule token = parse
  | space      { token lexbuf }
  | new_line   { Lexing.new_line lexbuf ; token lexbuf }
  | '"'        { Chars.reset () ; quoted_string lexbuf }
  | '{'        { LBRC }
  | '}'        { RBRC }
  | "/*"       { comment lexbuf }
  | ident as s { IDENT s }
  | eof        { EOF }
  | _ as c     { raise (Unrecognized_char c) }

and quoted_string = parse
  | "\\\"" { Chars.add '"' ; quoted_string lexbuf }
  | "\\\\" { Chars.add '\\' ; quoted_string lexbuf }
  | '"'    { STRING (Chars.get ()) }
  | _ as c { Chars.add c ; quoted_string lexbuf }

and comment = parse
  | "*/" { CommentLevel.decrease () ;
	   if CommentLevel.is_null () then token lexbuf
	   else comment lexbuf }
  | "/*" { CommentLevel.increase () ; comment lexbuf }
  | _    { comment lexbuf }


{

}
