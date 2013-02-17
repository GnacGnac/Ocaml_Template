
type t = { line : int ; char : int }

let make line char = { line ; char }

let line pos = pos.line
let char pos = pos.char


let of_buffer lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  make line char
