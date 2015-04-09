
open Result


type out_of_bounds = [`Out_of_bounds of int]

let enumerate_table values length f =
  let table = Hashtbl.create length in
  let f' i a = 
    let (key, value) = f i a in
    Hashtbl.add table key value in
  List.iteri f' values ;
  table

let to_int values length =
  Hashtbl.find (enumerate_table values length (fun i a -> (a, i)))

let of_int values length =
  let table = enumerate_table values length (fun i a -> (i, a)) in
  fun i ->
    try return (Hashtbl.find table i)
    with Not_found -> error (`Out_of_bounds i)

let int_conversions values =
  let length = List.length values in
  (to_int values length, of_int values length)
