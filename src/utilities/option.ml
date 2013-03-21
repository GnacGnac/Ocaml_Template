
let map f = function
  | None -> None
  | Some a -> Som (f a)
