
type out_of_bounds = [`Out_of_bounds of int]

val int_conversions :
  'a list ->
  ('a -> int) *
  (int -> ('a, [> out_of_bounds]) Result.t)
