
module Map = Map_ext.Make (String)


let repeat n s = List.fold_left (^) "" (List_ext.repeat n s)
