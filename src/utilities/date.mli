
module Week_day : sig
  include Comparison.S

  val mon : t
  val tue : t
  val wed : t
  val thu : t
  val fri : t
  val sat : t
  val sun : t

end

val is_leap : int -> bool

include Comparison.S
