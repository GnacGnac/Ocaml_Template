
module Generic : sig

  module type NODE = sig
    type t
    val to_string : t -> (string, [> `Node_not_bound_to_a_string of t]) Result.t
  end

  module type S = sig
    module Node : NODE
    type t = Int of int | Text of string | Node of Node.t * t list
    val int : int -> t
    val text : string -> t
    val node : Node.t -> t list -> t
    val to_string :
      t -> (string, [> `Node_not_bound_to_a_string of Node.t]) Result.t
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
  val to_string : t -> string
end

module ChildrenSpec : sig

  module type S = sig
    type node
    module NodeMap : Map_ext.S with type key = node
    type t
    val make : Occurrence.t -> Occurrence.t -> Occurrence.t NodeMap.t -> t
    val check :
      node -> t -> int -> int -> int NodeMap.t ->
      (unit,
       [> `Bad_int_occurrence of node * int * Occurrence.t
        | `Bad_text_occurrence of node * int * Occurrence.t
        | `Bad_sub_node_occurrence of node * node * int * Occurrence.t])
	Result.t
  end

  module Make (Node : Map_ext.ORDERED_TYPE) : S with type node = Node.t

end

module Instance : sig

  module type SPEC = sig
    include Stringable.S
    module Children : ChildrenSpec.S with type node = t
    val spec : t -> Children.t
    val possible_roots : Set.t
  end

  module type S = sig
    include Generic.S
    val parse :
    string ->
      (t,
       [> `Bad_int_occurrence of Node.t * int * Occurrence.t
        | `Bad_sub_node_occurrence of Node.t * Node.t * int * Occurrence.t
        | `Bad_text_occurrence of Node.t * int * Occurrence.t
        | `File_does_not_exist of string
        | `Could_not_open_file of string
        | `Unrecognized_char of char * Position.t
        | `Parse_error of Position.t
        | `Not_a_root_node of Node.t option
        | `Unrecognized_node of string]) Result.t
    val save :
      string -> t ->
      (unit,
       [> `Could_not_save_in_file of string
        | `Node_not_bound_to_a_string of Node.t]) Result.t
  end

  module Make (Spec : SPEC) : S with type Node.t = Spec.t

end
