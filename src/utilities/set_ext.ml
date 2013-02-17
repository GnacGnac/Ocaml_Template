
module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  val of_list : elt list -> t
  val to_list : t -> elt list
  val disjoint : t -> t -> bool
  val is_subset : t -> t -> bool
  val union_list : t list -> t
end

module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
  module M = Set.Make (Ord)
  include M

  let to_list = elements
  let of_list l = List.fold_right M.add l M.empty
  let disjoint s1 s2 = M.inter s1 s2 = M.empty

  let union_list = List.fold_left union empty

  let is_subset set1 set2 =
    let f x res = res && (mem x set2) in
    fold f set1 true
end
