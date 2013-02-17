
type ('a, 'b) t = Ok of 'a | Error of 'b

let return a = Ok a
let error e = Error e

let (>>=) a f = match a with
  | Ok a -> f a
  | Error b -> Error b
