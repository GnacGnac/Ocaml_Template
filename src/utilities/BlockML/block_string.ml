
open Result


module S = struct
  type t = string
  let to_string s = return s
end

include Generic.Make (S)
