
module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S

  val of_list : elt list -> t
  val to_list : t -> elt list
  val disjoint : t -> t -> bool
  val is_subset : t -> t -> bool
  val union_list : t list -> t
end

module Make (Ord : OrderedType) : S with type elt = Ord.t
