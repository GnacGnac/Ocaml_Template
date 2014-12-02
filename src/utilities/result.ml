
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

let fold ok_result error_result = function
  | Ok a -> ok_result a
  | Error err -> error_result err

let get error_result = fold (fun a -> a) error_result

let to_option = function
  | Ok a -> Some a
  | Error _ -> None

let of_option none_error = function
  | None -> error none_error
  | Some a -> return a

let safe_try f error_handler =
  try return (f ()) with exn -> error (error_handler exn)

let extract = function
  | Ok a -> a
  | _ -> assert false (* Only use on valid monads. *)

let (|>) f g = fun x -> g (f x)

let (@@) = Pervasives.(@@)

let (|-) = Pervasives.(|>)

let (|>>) a f = a >>= (f |> return)

let rec fun_pow f i a = if i <= 0 then a else fun_pow f (i - 1) (f a)

let rec iterate next continue a =
  if continue a then iterate next continue (next a) else a
