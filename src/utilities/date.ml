
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

  let next week_day = (succ week_day) mod 7
  let to_int i = i
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
let extract_make year month day = extract (make year month day)


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


let iterate_date f (date1, date2) =
  let next (date, days) =
    let (next_date, added_days) = f date in
    (next_date, days + added_days) in
  let continue (date, _) = date <= date2 in
  iterate next continue (date1, 0)

let next_year date =
  let open Pervasives in
  let year = year date in
  let next_year = year + 1 in
  let month = month date in
  let day = day date in
  let next_day = if month = 2 && day = 29 then 28 else day in
  let days =
    if (month >= 3 && is_leap next_year) ||
       (month <= 2 && day <> 29 && is_leap year) then
      366
    else 365 in
  (extract_make next_year month next_day, days)

let iterate_years = iterate_date next_year

let next_month date =
  let open Pervasives in
  let year = year date in
  let month = month date in
  let (next_year, next_month) =
    if month = 12 then (year + 1, 1)
    else (year, month + 1) in
  let day = day date in
  let last_day_next_month = number_of_days year next_month in
  let next_day = min day last_day_next_month in
  let days = number_of_days year month - (day - next_day) in
  (extract_make next_year next_month next_day, days)

let iterate_months = iterate_date next_month

let next_day date = (next date, 1)

let iterate_days = iterate_date next_day

let diff_ordered date1 date2 =
  let f (date1, days) iterate =
    let (date1, added_days) = iterate (date1, date2) in
    (date1, days + added_days) in
  let iterates = [iterate_years ; iterate_months ; iterate_days] in
  snd (List.fold_left f (date1, 0) iterates)

let diff date1 date2 =
  if date1 <= date2 then - (diff_ordered date1 date2)
  else diff_ordered date2 date1


let week_day date =
  let base_date = extract_make 1900 1 1 in
  diff date base_date mod 7


type format = Year | Month | Day

let format_to_item = function
  | Year -> year
  | Month -> month
  | Day -> day

let to_string ?(format=[Year ; Month ; Day]) ?(sep="-") date =
  let items = List.map (fun format -> format_to_item format date) format in
  let f i = (if Pervasives.(<) i 10 then "0" else "") ^ (string_of_int i) in
  List_ext.to_string sep f items
