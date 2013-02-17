
open Result


module type ASSOC = sig
  type t
  val compare : t -> t -> int
  val get : (t * string) list
end


module type S = sig
  type t
  val of_string :
    string -> (t, [> `Unrecognized_string_assoc of string]) Result.t
  val mem       : string -> bool
  val to_string : t -> (string, [> `Unrecognized_elt_assoc of t]) Result.t
  module Set : Set_ext.S with type elt = t
  val all : Set.t
end


module Make (A : ASSOC) = struct

  include A

  let get = List.map (fun (a, s) -> (a, String.lowercase s)) A.get

  module Set = Set_ext.Make(A)

  module B = Bijection.Make (A) (String)

  let bijection = B.of_list get

  let of_string s =
    let f_error = function `Not_found -> `Unrecognized_string_assoc s in
    map_error f_error (B.find2 s bijection)

  let mem s = B.mem2 s bijection

  let to_string element =
    let f_error = function `Not_found -> `Unrecognized_elt_assoc element in
    map_error f_error (B.find1 element bijection)

  let all = Set.of_list (List.map fst get)

end
