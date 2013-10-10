
type ('a, 'b) t = Ok of 'a | Error of 'b

let return a = Ok a
let error e = Error e

let (>>=) a f = match a with
  | Ok a -> f a
  | Error b -> Error b

let map f_ok f_error = function
  | Ok a -> Ok (f_ok a)
  | Error error -> Error (f_error error)

let map_result f_ok = map f_ok (fun a -> a)

let map_error f_error = map (fun a -> a) f_error

let to_bool = function
  | Ok a -> Some a
  | Error _ -> None

let extract = function
  | Ok a -> a
  | _ -> assert false (* Only use on valid monads. *)

let (|>) f g = fun x -> g (f x)
