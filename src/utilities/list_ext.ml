
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


let repeat n a =
  let rec aux acc i = if i <= 0 then acc else aux (a :: acc) (i - 1) in
  aux [] n


let foldi f a l =
  let f' (index, res) e = (index + 1, f index res e) in
  snd (List.fold_left f' (0, a) l)


let mapi f l =
  let f' index res e = (f index e) :: res in
  List.rev (foldi f' [] l)

let assoc e l =
  try return (List.assoc e l)
  with Not_found -> error `Not_found

let removei i =
  let f j res item = res @ (if j = i then [] else [item]) in
  foldi f []

let map_nth i f =
  let f' j a = (if j = i then f else (fun a -> a)) a in
  mapi f'
