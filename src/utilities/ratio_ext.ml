
open Result
open Big_int


type t = { num : big_int ; den : big_int }

let num ratio = ratio.num
let den ratio = ratio.den

let make num den = { num ; den }


let zero = make zero_big_int unit_big_int

let is_null ratio = eq_big_int (num ratio) zero_big_int

let add ratio1 ratio2 =
  let num1 = mult_big_int (num ratio1) (den ratio2) in
  let num2 = mult_big_int (num ratio2) (den ratio1) in
  make (add_big_int num1 num2) (mult_big_int (den ratio1) (den ratio2))

let sub ratio1 ratio2 =
  let num1 = mult_big_int (num ratio1) (den ratio2) in
  let num2 = mult_big_int (num ratio2) (den ratio1) in
  make (sub_big_int num1 num2) (mult_big_int (den ratio1) (den ratio2))

let mul ratio1 ratio2 =
  let num = mult_big_int (num ratio1) (num ratio2) in
  let den = mult_big_int (den ratio1) (den ratio2) in
  make num den

let div ratio1 ratio2 =
  if is_null ratio2 then error `Division_by_zero
  else
    let n = mult_big_int (num ratio1) (den ratio2) in
    let d = mult_big_int (den ratio1) (num ratio2) in
    return (make n d)

let of_int i = make (big_int_of_int i) unit_big_int

let to_string ratio = string_of_big_int (div_big_int (num ratio) (den ratio))
