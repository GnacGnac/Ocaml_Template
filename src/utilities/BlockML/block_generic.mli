
module type NODE = sig
  type t
  val to_string : t -> string
end

module type S = sig
  module Node : NODE
  type primitive = Int of int | Text of string
  type contents = Primitive of primitive | Node of Node.t * t list
  and t = contents Position.t

  type no_int = [`No_int]
  type no_text = [`No_text]
  type no_such_child = [`No_such_child]
  type no_children = [`No_children]
  type not_a_node = [`Not_a_node]

  val int_content : int -> contents
  val text_content : string -> contents
  val node_content : Node.t -> t list -> contents
  val int : int -> t
  val text : string -> t
  val node : Node.t -> t list -> t
  val get_int : t -> (int, [> no_int]) Result.t
  val get_int_with_pos : t -> (int Position.t, [> no_int]) Result.t
  val get_text : t -> (string, [> no_text]) Result.t
  val get_text_with_pos : t -> (string Position.t, [> no_text]) Result.t
  val get_node : Node.t -> t -> (t, [> no_such_child]) Result.t
  val get_node_no_pos : Node.t -> t -> (contents, [> no_such_child]) Result.t
  val get_int_children : t -> (int list, [> no_children]) Result.t
  val get_int_children_with_pos :
    t -> (int Position.t list, [> no_children]) Result.t
  val get_text_children : t -> (string list, [> no_children]) Result.t
  val get_text_children_with_pos :
    t -> (string Position.t list, [> no_children]) Result.t
  val get_node_children : Node.t -> t -> (t list, [> no_children]) Result.t
  val get_node_children_no_pos :
    Node.t -> t -> (contents list, [> no_children]) Result.t
  val get_root_node : t -> (Node.t, [> not_a_node]) Result.t
  val get_root_node_with_pos :
    t -> (Node.t Position.t, [> not_a_node]) Result.t
  val get_children : t -> (t list, [> no_children]) Result.t
  val get_children_no_pos : t -> (contents list, [> no_children]) Result.t

  val to_string : t -> string

  (* Unsafe functions: raise assertion failure. *)
  val extract_int : t -> int
  val extract_int_with_pos : t -> int Position.t
  val extract_text : t -> string
  val extract_text_with_pos : t -> string Position.t
  val extract_node : Node.t -> t -> t
  val extract_node_no_pos : Node.t -> t -> contents
  val extract_int_children : t -> int list
  val extract_int_children_with_pos : t -> int Position.t list
  val extract_text_children : t -> string list
  val extract_text_children_with_pos : t -> string Position.t list
  val extract_node_children : Node.t -> t -> t list
  val extract_node_children_no_pos : Node.t -> t -> contents list
  val extract_root_node : t -> Node.t
  val extract_root_node_with_pos : t -> Node.t Position.t
  val extract_children : t -> t list
  val extract_children_no_pos : t -> contents list
end

module Make (N : NODE) : S with module Node = N
