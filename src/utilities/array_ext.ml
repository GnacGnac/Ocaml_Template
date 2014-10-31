
open Result


let make_matrix dimx dimy e =
  let a = Array.make_matrix dimx dimy e in
  let row () = Array.make dimy e in
  let rec aux i = if i < dimx then (a.(i) <- row () ; aux (i + 1)) in
  aux 0 ;
  a

let foldi f e a =
  let length = Array.length a in
  let rec aux e i =
    if i >= length then e
    else aux (f i e a.(i)) (i + 1) in
  aux e 0


type out_of_bounds = [`Out_of_bounds]

let get array i =
  if i < Array.length array then return array.(i) else error `Out_of_bounds
