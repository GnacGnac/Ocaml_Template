
open Result


module type ORDERED_TYPE = sig
  include Map.OrderedType
end


type not_found = [`Not_found]


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
  val foldi : (int -> key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
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
  val find : key -> 'a t -> ('a, not_found) Result.t
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


module Make (Ord : ORDERED_TYPE) : S with type key = Ord.t = struct

  include Map.Make (Ord)

  let foldi f map b =
    let f' key a (i, b) = (i + 1, f i key a b) in
    snd (fold f' map (0, b))

  let find key map =
    try return (find key map)
    with Not_found -> error `Not_found

  let merge_f f map1 map2 =
    let f_merge _ e1 e2 = match e1, e2 with
      | None, None -> None
      | Some e, None | None, Some e -> Some e
      | Some e1, Some e2 -> Some (f e1 e2) in
    merge f_merge map1 map2

  let split_couple map =
    let f key (a, b) (resa, resb) = (add key a resa, add key b resb) in
    fold f map (empty, empty)

  let combine mapa mapb =
    let f key a res = match find key mapb with
      | Ok img -> add key (a, img) res
      | Error `Not_found -> res in
    fold f mapa empty

  let of_list l =
    let f map (key, binding) = add key binding map in
    List.fold_left f empty l

  let to_list = bindings

  let to_string f_key f_a entry_sep couple_sep map =
    let f key a s = s ^ (f_key key) ^ couple_sep ^ (f_a a) ^ entry_sep in
    fold f map ""

end


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
  val find : key -> t -> (img, not_found) Result.t
  val map : (img -> img) -> t -> t
  val mapi : (key -> img -> img) -> t -> t
  val of_list : (key * img) list -> t
  val to_list : t -> (key * img) list
  val to_string :
    (key -> string) -> (img -> string) -> string -> string -> t -> string
end


module Make1 (Key : ORDERED_TYPE) (Img : ORDERED_TYPE)
  : S1 with type key = Key.t and type img = Img.t =
struct

  module M = Make (Key)

  type key = M.key
  type img = Img.t
  type t = img M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let mem = M.mem
  let add = M.add
  let singleton = M.singleton
  let remove = M.remove
  let merge_f = M.merge_f
  let merge = M.merge
  let compare = M.compare Img.compare
  let equal = M.equal (fun img1 img2 -> Img.compare img1 img2 = 0)
  let iter = M.iter
  let fold = M.fold
  let for_all = M.for_all
  let exists = M.exists
  let filter = M.filter
  let partition = M.partition
  let cardinal = M.cardinal
  let bindings = M.bindings
  let min_binding = M.min_binding
  let max_binding = M.max_binding
  let choose = M.choose
  let split = M.split
  let find = M.find
  let map = M.map
  let mapi = M.mapi
  let of_list = M.of_list
  let to_list = M.to_list
  let to_string = M.to_string

end
