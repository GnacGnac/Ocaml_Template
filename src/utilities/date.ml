
type week_day = int

let mon = 0
let tue = 1
let wed = 2
let thu = 3
let fri = 4
let sat = 5
let sun = 6

let divides a b = a mod b = 0
let is_leap year =
  ((divides year 4) && not (divides year 100)) ||
  (divides year 400)
