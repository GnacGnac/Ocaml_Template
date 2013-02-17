
module type ORDERED_TYPE = sig
  include Map.OrderedType
end

module type S = sig
  type key
  type +'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a ) list
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val choose : 'a t -> key * 'a
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> ('a, [> `Not_found]) Result.t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

  val merge_f : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val split_couple : ('a * 'b) t -> 'a t * 'b t
  val combine : 'a t -> 'b t -> ('a * 'b) t
  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val to_string :
    (key -> string) -> ('a -> string) -> string -> string -> 'a t -> string
end

module Make (Ord : ORDERED_TYPE) : S with type key = Ord.t

module type S1 = sig
  type key
  type img
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : key -> t -> bool
  val add : key -> img -> t -> t
  val singleton : key -> img -> t
  val remove : key -> t -> t
  val merge_f : (img -> img -> img) -> t -> t -> t
  val merge : (key -> img option -> img option -> img option) -> t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val iter : (key -> img -> unit) -> t -> unit
  val fold : (key -> img -> 'b -> 'b) -> t -> 'b -> 'b
  val for_all : (key -> img -> bool) -> t -> bool
  val exists : (key -> img -> bool) -> t -> bool
  val filter : (key -> img -> bool) -> t -> t
  val partition : (key -> img -> bool) -> t -> t * t
  val cardinal : t -> int
  val bindings : t -> (key * img) list
  val min_binding : t -> key * img
  val max_binding : t -> key * img
  val choose : t -> key * img
  val split : key -> t -> t * img option * t
  val find : key -> t -> (img, [> `Not_found]) Result.t
  val map : (img -> img) -> t -> t
  val mapi : (key -> img -> img) -> t -> t
  val of_list : (key * img) list -> t
  val to_list : t -> (key * img) list
  val to_string :
    (key -> string) -> (img -> string) -> string -> string -> t -> string
end

module Make1 (Key : ORDERED_TYPE) (Img : ORDERED_TYPE)
  : S1 with type key = Key.t and type img = Img.t
