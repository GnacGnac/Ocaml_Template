
open Result


let rec to_string_err sep f = function
  | [] -> return ""
  | [e] -> f e
  | e :: l ->
    f e >>= fun hd ->
    to_string_err sep f l >>= fun tail ->
    return (hd ^ sep ^ tail)

let to_string sep f l =
  let f' e = return (f e) in
  match to_string_err sep f' l with
    | Ok s -> s
    | Error _ -> assert false (* Impossible: [f'] cannot an error. *)


let bind f =
  let rec aux acc = function
    | [] -> acc
    | e :: l -> aux (acc >>= fun l -> f e >>= fun e -> return (l @ [e])) l in
  aux (return [])
