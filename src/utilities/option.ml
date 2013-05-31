
let map f = function
  | None -> None
  | Some a -> Some (f a)

let extract = function
  | None -> assert false (* do not use on this argument *)
  | Some a -> a
