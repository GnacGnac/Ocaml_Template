
open Result


let map f = function
  | None -> None
  | Some a -> Some (f a)

let extract = function
  | None -> assert false (* do not use on this argument *)
  | Some a -> a

let of_result = Result.to_option
let to_result = Result.of_option

let fold none_result some_result = function
  | None -> none_result
  | Some a -> some_result a

let get default = fold default (fun a -> a)
