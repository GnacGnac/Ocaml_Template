
module Week_day : sig
  include Comparison.S

  val mon : t
  val tue : t
  val wed : t
  val thu : t
  val fri : t
  val sat : t
  val sun : t

  val to_int : t -> int
end


include Comparison.S

val is_leap : int -> bool
val number_of_days : int -> int -> int
val week_day : t -> Week_day.t

type make_error = [
| `Invalid_date_month_lower_than_1 of int
| `Invalid_date_month_higher_than_12 of int
| `Invalid_date_day_lower_than_1 of int
| `Invalid_date_day_higher_than_last of (int * int * int * int)
]
val make : int -> int -> int -> (t, [> make_error]) Result.t

val next : t -> t
val diff : t -> t -> int

type format = Year | Month | Day
val to_string : ?format:(format list) -> ?sep:string -> t -> string
