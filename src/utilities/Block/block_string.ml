
open Result


module S = struct
  type t = string
  let to_string s = return s
end

include Block_generic.Make (S)
