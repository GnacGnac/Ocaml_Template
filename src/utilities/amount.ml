
open Result
open Big_int


type t = { num : big_int ; den : big_int }

let num amount = amount.num
let den amount = amount.den

let make num den = { num ; den }


let zero = make zero_big_int unit_big_int

let add amount1 amount2 =
  let num1 = mult_big_int (num amount1) (den amount2) in
  let num2 = mult_big_int (num amount2) (den amount1) in
  make (add_big_int num1 num2) (mult_big_int (den amount1) (den amount2))

let sub amount1 amount2 =
  let num1 = mult_big_int (num amount1) (den amount2) in
  let num2 = mult_big_int (num amount2) (den amount1) in
  make (sub_big_int num1 num2) (mult_big_int (den amount1) (den amount2))

let mul amount1 amount2 =
  let num = mult_big_int (num amount1) (num amount2) in
  let den = mult_big_int (den amount1) (den amount2) in
  make num den

let div amount1 amount2 =
  let n = mult_big_int (num amount1) (den amount2) in
  let d = mult_big_int (den amount1) (num amount2) in
  make n d

let to_int i =
  let error = error (`Not_an_int i) in
  if not (eq_big_int (den i) zero_big_int) then
    let amount = div_big_int (num i) (den i) in
    if is_int_big_int amount then return (int_of_big_int amount)
    else error
  else error
let of_int i = make (big_int_of_int i) unit_big_int

let of_string s =
  try return (make (big_int_of_string s) unit_big_int)
  with _ (* what's the exception thrown? *) -> error (`Not_an_amount s)

let to_string amount =
  let s = string_of_big_int (div_big_int (num amount) (den amount)) in
  let l = String.length s in
  if l <= 1 then "0.0" ^ s
  else
    if l <= 2 then "0." ^ s
    else
      let i = String.length s - 2 in
      (String.sub s 0 i) ^ "." ^ (String.sub s i 2)
