
open Result
open Big_int


type t = { num : big_int ; den : big_int }

let num amount = amount.num
let den amount = amount.den

let make num den =
  if den = zero_big_int then
    (Printf.eprintf "Division by zero.\n%!" ;
     assert false)
  else { num ; den }


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

let remove_spaces_point_comma s = String_ext.remove_chars s [' ' ; '.' ; ',']

let add_zeros error s =
  let l = String.length s in
  try
    let p =
      try String.index s '.'
      with Not_found -> String.index s ',' in
    if p = l - 1 then return (s ^ "00")
    else
      if p = l - 2 then return (s ^ "0")
      else
	if p = l - 3 then return s
	else error
  with Not_found -> return (s ^ "00")

let of_string s =
  let error = error (`Not_an_amount s) in
  add_zeros error s >>= fun s ->
  let s = remove_spaces_point_comma s in
  try return (make (big_int_of_string s) unit_big_int)
  with _ (* what's the thrown exception? *) -> error

let to_string amount =
  let amount = div_big_int (num amount) (den amount) in
  let (euros, cents) = quomod_big_int amount (big_int_of_int 100) in
  let euros = string_of_big_int euros in
  let cents =
    (if lt_big_int cents (big_int_of_int 10) then "0" else "") ^
    (string_of_big_int cents) in
  euros ^ (if cents = "00" then "" else "," ^ cents)
