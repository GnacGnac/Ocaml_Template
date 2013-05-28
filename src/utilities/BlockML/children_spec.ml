
open Result


module Occurrence = struct

  type bound = Int of int | Infty

  let string_of_bound = function
    | Int i -> string_of_int i
    | Infty -> "*"

  type t = { min : int ; max : bound }

  let make min max = { min ; max }

  let min occurrence = occurrence.min
  let max occurrence = occurrence.max

  let le_bound a = function
    | Int i -> a <= i
    | Infty -> true

  let mem a occurrence = (min occurrence) <= a & le_bound a (max occurrence)

  let exactly i = make i (Int i)
  let at_least i = make i Infty
  let at_most i = make 0 (Int i)
  let between i j = make i (Int j)
  let none = exactly 0
  let any = at_least 0
  let option = between 0 1
  let one = exactly 1

  let to_string occurrence =
    Printf.sprintf "%d..%s" (min occurrence) (string_of_bound (max occurrence))

end


module Primitive = struct

  type t = Int | Text

  type 'a specification = t -> 'a
  type occurrence_specification = Occurrence.t specification

  let none _ = Occurrence.none
  let anys _ = Occurrence.any
  let one_value value (prim : t) (prim' : t) =
    if prim' = prim then value else Occurrence.none
  let one = one_value Occurrence.one
  let any = one_value Occurrence.any
  let option = one_value Occurrence.option
  let one_int = one Int
  let one_text = one Text
  let any_int = any Int
  let any_text = any Text
  let option_int = option Int
  let option_text = option Text

end


type 'a unsafe_children_specification = ('a * Occurrence.t) list

let empty = []
let all occurrence list = List.map (fun a -> (a, occurrence)) list
let anys list = all Occurrence.any list
let ones list = all Occurrence.one list
let options list = all Occurrence.option list
let any a = anys [a]
let one a = ones [a]
let option a = options [a]


type 'node occurrence_error =
  [ `Bad_int_occurrence of 'node Position.t * int * Occurrence.t
  | `Bad_text_occurrence of 'node Position.t * int * Occurrence.t
  | `Bad_sub_node_occurrence of 'node Position.t * 'node * int * Occurrence.t]

module type S = sig
  type node
  module NodeMap : sig
    include Map_ext.S with type key = node
    val all : Occurrence.t -> node list -> Occurrence.t t
    val any : node -> Occurrence.t t
    val one : node -> Occurrence.t t
    val option : node -> Occurrence.t t
    val anys : node list -> Occurrence.t t
    val ones : node list -> Occurrence.t t
    val options : node list -> Occurrence.t t
  end
  type 'a specification
  val make : 'a Primitive.specification -> 'a NodeMap.t -> 'a specification
  type t = Occurrence.t specification
  val check :
    node Position.t -> t -> int specification ->
    (unit, [> node occurrence_error]) Result.t
end


module Make (Node : Map_ext.ORDERED_TYPE) = struct

  type node = Node.t

  type node_pos = node Position.t

  module NodeMap = struct 
    include Map_ext.Make (Node)
    let all i l = of_list (all i l)
    let anys = all Occurrence.any
    let ones = all Occurrence.one
    let options = all Occurrence.option
    let any node = anys [node]
    let one node = ones [node]
    let option node = options [node]
  end

  type 'a specification =
      { primitives : 'a Primitive.specification ;
	sub_nodes : 'a NodeMap.t }

  type t = Occurrence.t specification

  let make primitives sub_nodes = { primitives ; sub_nodes }

  let primitives children_spec = children_spec.primitives
  let sub_nodes children_spec = children_spec.sub_nodes

  let check_primitive nb_primitives primitives () (prim, prim_error) =
    let nb = nb_primitives prim in
    let spec = primitives prim in
    if Occurrence.mem nb spec then return ()
    else error (prim_error nb spec)

  let check_primitives_spec node nb_primitives primitives =
    List_ext.fold_bind (check_primitive nb_primitives primitives) ()
      [(Primitive.Int,
	(fun nb_ints ints -> `Bad_int_occurrence (node, nb_ints, ints))) ;
       (Primitive.Text,
	(fun nb_texts texts -> `Bad_text_occurrence (node, nb_texts, texts)))]

  let merge_spec node occurrence spec_occurrence =
    match occurrence, spec_occurrence with
      | None, None -> None
      | Some occurrence, None -> Some (occurrence, Occurrence.none)
      | None, Some spec_occurrence -> Some (0, spec_occurrence)
      | Some occurrence, Some spec_occurrence ->
	Some (occurrence, spec_occurrence)

  let check_sub_nodes_spec node nb_sub_nodes sub_nodes =
    let occurrences_and_specs =
      NodeMap.merge merge_spec nb_sub_nodes sub_nodes in
    let f sub_node (occurrence, spec_occurrence) res =
      res >>= fun () ->
      if Occurrence.mem occurrence spec_occurrence then return ()
      else
	error
	  (`Bad_sub_node_occurrence
	      (node, sub_node, occurrence, spec_occurrence))
    in
    NodeMap.fold f occurrences_and_specs (return ())

  let check node children_spec nb_children =
    let nb_primitives = primitives nb_children in
    let nb_sub_nodes = sub_nodes nb_children in
    let primitives = primitives children_spec in
    let sub_nodes = sub_nodes children_spec in
    check_primitives_spec node nb_primitives primitives >>= fun () ->
    check_sub_nodes_spec node nb_sub_nodes sub_nodes

end
