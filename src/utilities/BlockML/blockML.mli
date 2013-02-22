
module Generic : sig

  module type NODE = sig
    type t
    val to_string : t -> string
  end

  module type S = sig
    module Node : NODE
    type contents = Int of int | Text of string | Node of Node.t * t list
    and t = contents Position.t
    val int_content : int -> contents
    val text_content : string -> contents
    val node_content : Node.t -> t list -> contents
    val int : int -> t
    val text : string -> t
    val node : Node.t -> t list -> t
    val to_string : t -> string
  end

  module Make (N : NODE) : S with type Node.t = N.t

end

module Occurrence : sig
  type bound = Int of int | Infty
  type t
  val make : int -> bound -> t
  val any : t
  val none : t
  val exactly : int -> t
  val at_least : int -> t
  val at_most : int -> t
  val between : int -> int -> t
  val option : t
  val one : t
  val to_string : t -> string
end

module ChildrenSpec : sig

  module type S = sig
    type node
    type node_pos = node Position.t
    module NodeMap : Map_ext.S with type key = node
    type t
    val make : Occurrence.t -> Occurrence.t -> Occurrence.t NodeMap.t -> t
    val check :
      node_pos -> t -> int -> int -> int NodeMap.t ->
      (unit,
       [> `Bad_int_occurrence of node_pos * int * Occurrence.t
        | `Bad_text_occurrence of node_pos * int * Occurrence.t
        | `Bad_sub_node_occurrence of node_pos * node * int * Occurrence.t])
	Result.t
  end

  module Make (Node : Map_ext.ORDERED_TYPE) : S with type node = Node.t

end

module Instance : sig

  module type SPEC = sig
    type t
    module Set : Set_ext.S with type elt = t
    val to_string : t -> string
    val of_string : string -> (t, [> `Unrecognized_string of string]) Result.t
    module Children : ChildrenSpec.S with type node = t
    val spec : t -> Children.t
    val possible_roots : Set.t
  end

  module type S = sig
    include Generic.S
    type node_pos = Node.t Position.t
    val analyze :
      t ->
      (unit,
       [> `Bad_int_occurrence of node_pos * int * Occurrence.t
        | `Bad_sub_node_occurrence of node_pos * Node.t * int * Occurrence.t
        | `Bad_text_occurrence of node_pos * int * Occurrence.t
        | `Not_a_root_node of Node.t option Position.t]) Result.t
    val parse :
    string ->
      (t,
       [> `Bad_int_occurrence of node_pos * int * Occurrence.t
        | `Bad_sub_node_occurrence of node_pos * Node.t * int * Occurrence.t
        | `Bad_text_occurrence of node_pos * int * Occurrence.t
        | `File_does_not_exist of string
        | `Could_not_open_file of string
        | `Unrecognized_char of char Position.t
        | `Parse_error of unit Position.t
        | `Not_a_root_node of Node.t option Position.t
        | `Unrecognized_node of string Position.t]) Result.t
    val save :
      string -> t -> (unit, [> `Could_not_save_in_file of string]) Result.t
  end

  module Make (Spec : SPEC) : S with type Node.t = Spec.t

end
