
module type S = sig
  type a
  type b
  type t
  val compare : t -> t -> int
  val empty : t
  val add : a -> b -> t -> t
  val find1 : a -> t -> (b, [> `Not_found]) Result.t
  val find2 : b -> t -> (a, [> `Not_found]) Result.t
  val mem2 : b -> t -> bool
  val of_list : (a * b) list -> t
end


module Make (A : Map_ext.ORDERED_TYPE) (B : Map_ext.ORDERED_TYPE) = struct

  type a = A.t
  type b = B.t

  module MapA = Map_ext.Make1 (A) (B)
  module MapB = Map_ext.Make1 (B) (A)

  type t = (MapA.t * MapB.t)

  let compare (map_a1, map_b1) (map_a2, map_b2) =
    let res1 = MapA.compare map_a1 map_a2 in
    if res1 <> 0 then res1
    else MapB.compare map_b1 map_b2

  let empty = (MapA.empty, MapB.empty)

  let add a b (map_a, map_b) = (MapA.add a b map_a, MapB.add b a map_b)

  let find1 a (map_a, _) = MapA.find a map_a
  let find2 b (_, map_b) = MapB.find b map_b

  let mem2 b (_, map_b) = MapB.mem b map_b

  let of_list =
    let f bijection (a, b) = add a b bijection in
    List.fold_left f empty

end
