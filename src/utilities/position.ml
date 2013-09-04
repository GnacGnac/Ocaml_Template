
open Result


module Position = struct

  type t = { file : string ; line : int ; char : int }
  let make file line char = { file ; line ; char }
  let file pos = pos.file
  let line pos = pos.line
  let char pos = pos.char
  let all pos = (file pos, line pos, char pos)

end

type 'a t = { contents : 'a ; pos : Position.t option }

let make contents file line char =
  { contents ; pos = Some (Position.make file line char) }

let make_dummy contents = { contents ; pos = None }

let contents pos = pos.contents
let all pos = match pos.pos with
  | None -> error `No_position
  | Some pos -> return (Position.all pos)
let file pos =
  all pos >>= fun (file, _, _) -> return file
let line pos =
  all pos >>= fun (_, line, _) -> return line
let char pos =
  all pos >>= fun (_, _, char) -> return char


let change_contents contents pos = { pos with contents }


let apply f_pos f_no_pos pos =
  (match pos.pos with
    | None -> f_no_pos
    | Some all ->
      f_pos (Position.file all) (Position.line all) (Position.char all))
  (contents pos)

let map_contents f pos = change_contents (f (contents pos)) pos
