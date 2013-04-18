
module Set = Set_ext.Make (String)

module Map = Map_ext.Make (String)


let repeat n s = List.fold_left (^) "" (List_ext.repeat n s)


let remove_chars s cl =
  let l = String.length s in
  let rec aux i =
    if i >= l then ""
    else
      if List.mem s.[i] cl then aux (i+1)
      else (String.make 1 s.[i]) ^ (aux (i+1)) in
  aux 0
