
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
    | Error _ -> assert false (* Impossible: [f'] cannot raise an error. *)


let fold_bind f e l =
  let f' res e' = res >>= fun res -> f res e' in
  List.fold_left f' (return e) l

let bind f l = fold_bind (fun l e -> f e >>= fun e -> return (l @ [e])) [] l
