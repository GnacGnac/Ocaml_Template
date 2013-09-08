
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
    else aux (f i e) (i + 1) in
  aux e 0
