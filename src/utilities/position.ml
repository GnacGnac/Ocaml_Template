
open Result


module Position = struct

  type t = { line : int ; char : int }
  let make line char = { line ; char }
  let line pos = pos.line
  let char pos = pos.char

end

type 'a t = { contents : 'a ; pos : Position.t option }

let make contents line char =
  { contents ; pos = Some (Position.make line char) }

let make_dummy contents = { contents ; pos = None }

let contents pos = pos.contents
let line pos = match pos.pos with
  | None -> error `No_position
  | Some pos -> return (Position.line pos)
let char pos = match pos.pos with
  | None -> error `No_position
  | Some pos -> return (Position.char pos)


let change_contents contents pos = { pos with contents }


let of_buffer_offset lexbuf contents offset =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - offset in
  make contents line char

let of_buffer lexbuf contents = of_buffer_offset lexbuf contents 0


let apply f_pos f_no_pos pos =
  (match pos.pos with
    | None -> f_no_pos
    | Some line_and_char ->
      f_pos (Position.line line_and_char) (Position.char line_and_char))
  (contents pos)

let map_contents f pos = change_contents (f (contents pos)) pos
