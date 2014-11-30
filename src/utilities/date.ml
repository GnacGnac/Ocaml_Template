
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

let is_leap year =
  ((divides year 4) && not (divides year 100)) ||
  (divides year 400)


module M = struct
  type t = { day : int ; month : int ; year : int }

  let day date = date.day
  let month date = date.month
  let year date = date.year

  let to_lex date = [year date ; month date ; day date]

  let compare date1 date2 =
    List_ext.lex_compare Pervasives.(compare) (to_lex date1) (to_lex date2)
end

include Comparison.Make (M)
