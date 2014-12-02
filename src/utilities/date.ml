
open Result


module Week_day = struct
  module M = struct
    type t = int
    let compare = Pervasives.compare
  end

  include Comparison.Make (M)

  let mon = 0
  let tue = 1
  let wed = 2
  let thu = 3
  let fri = 4
  let sat = 5
  let sun = 6
end


let divides a b = a mod b = 0

let is_even a = divides a 2
let is_odd = is_even |> not

let is_leap year =
  ((divides year 4) && not (divides year 100)) ||
  (divides year 400)

let number_of_days year month =
  let open Pervasives in
  match month with
  | 2 when is_leap year -> 29
  | 2 -> 28
  | _ when (is_odd month && month <= 7) || (is_even month && month >= 8) -> 31
  | _ -> 30


type make_error = [
| `Invalid_date_month_lower_than_1 of int
| `Invalid_date_month_higher_than_12 of int
| `Invalid_date_day_lower_than_1 of int
| `Invalid_date_day_higher_than_last of (int * int * int * int)
]

module M = struct
  type t = { year : int ; month : int ; day : int }

  let year date = date.year
  let month date = date.month
  let day date = date.day

  let error_month_lower_than_1 (year, month, day) =
    `Invalid_date_month_lower_than_1 month
  let error_month_higher_than_12 (year, month, day) =
    `Invalid_date_month_higher_than_12 month
  let error_day_lower_than_1 (year, month, day) =
    `Invalid_date_day_lower_than_1 day
  let error_day_higher_than_last (year, month, day) =
    `Invalid_date_day_higher_than_last
       (year, month, day, number_of_days year month)

  let check_month_1 (year, month, day) = 1 <= month
  let check_month_12 (year, month, day) = month <= 12
  let check_day_1 (year, month, day) = 1 <= day
  let check_day_last (year, month, day) = day <= number_of_days year month

  let checks =
    [(check_month_1, error_month_lower_than_1) ;
     (check_month_12, error_month_higher_than_12) ;
     (check_day_1, error_day_lower_than_1) ;
     (check_day_last, error_day_higher_than_last)]

  let make year month day =
    let check_one date (check, date_error) =
      if check date then return date else error (date_error date) in
    List_ext.fold_bind check_one (year, month, day) checks |>>
      fun (year, month, day) ->
    { year ; month ; day }

  let to_lex date = [year date ; month date ; day date]

  let compare date1 date2 =
    List_ext.lex_compare Pervasives.(compare) (to_lex date1) (to_lex date2)
end

include Comparison.Make (M)


let year = M.year
let month = M.month
let day = M.day
let make = M.make


let next date =
  let year = year date in
  let month = month date in
  let day = day date in
  let last_day = number_of_days year month in
  let next_date =
    if day = last_day then
      if month = 12 then make (year + 1) 1 1
      else make year (month + 1) 1
    else make year month (day + 1) in
  extract next_date


type format = Year | Month | Day

let format_to_item = function
  | Year -> year
  | Month -> month
  | Day -> day

let to_string ?(format=[Year ; Month ; Day]) ?(sep="-") date =
  let items = List.map (fun format -> format_to_item format date) format in
  let f i = (if Pervasives.(<) i 10 then "0" else "") ^ (string_of_int i) in
  List_ext.to_string sep f items
