
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
    val get_int : t list -> (int Position.t, [> `No_int]) Result.t
    val get_text : t list -> (string Position.t, [> `No_text]) Result.t
    val get_child :
      Node.t -> t list -> (t list Position.t, [> `No_such_child]) Result.t
    val to_string : t -> string

    (* Unsafe functions: raises assertion failure. *)
    val extract_get : (t list -> ('a Position.t, 'b) Result.t) -> t list -> 'a
    val extract_int : t list -> int
    val extract_text : t list -> string
    val extract_child : Node.t -> t list -> t list
    val extract_node : Node.t -> t -> t list
  end

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

type ('node, 'node_pos) occurrence_error =
  [ `Bad_int_occurrence of 'node_pos * int * Occurrence.t
  | `Bad_text_occurrence of 'node_pos * int * Occurrence.t
  | `Bad_sub_node_occurrence of 'node_pos * 'node * int * Occurrence.t]

module ChildrenSpec : sig

  module type S = sig
    type node
    type node_pos = node Position.t
    module NodeMap : Map_ext.S with type key = node
    type t
    val make : Occurrence.t -> Occurrence.t -> Occurrence.t NodeMap.t -> t
    val check :
      node_pos -> t -> int -> int -> int NodeMap.t ->
      (unit, [> (node, node_pos) occurrence_error]) Result.t
  end

  module Make (Node : Map_ext.ORDERED_TYPE) : S with type node = Node.t
  module MakeUnsafe (Node : sig type t end) : S with type node = Node.t

end

type ('node, 'node_pos) analyze_error =
  [ ('node, 'node_pos) occurrence_error
  | `Not_a_root_node of 'node option Position.t]

type ('node, 'node_pos) parse_error =
  [ ('node, 'node_pos) analyze_error
  | `File_does_not_exist of string
  | `Could_not_open_file of string
  | `Unrecognized_char of char Position.t
  | `Parse_error of unit Position.t
  | `Unrecognized_node of string Position.t]

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
    val analyze : t -> (unit, [> (Node.t, node_pos) analyze_error]) Result.t
    val parse : string -> (t, [> (Node.t, node_pos) parse_error]) Result.t
    val save :
      string -> t -> (unit, [> `Could_not_save_in_file of string]) Result.t
  end

  module Make (Spec : SPEC) : S with type Node.t = Spec.t

end

module UnsafeInstance : sig

  module type SPEC = sig
    type t
    val node_string : (t * string) list
    module Children : ChildrenSpec.S with type node = t
    val spec : t -> Children.t
    val possible_roots : t list
  end

  module Make (Spec : SPEC) : Instance.S with type Node.t = Spec.t

end


type grammar_node =
  | Grammar
  | Possible_roots
  | Cardinalities
  | Cardinality
  | Parent
  | Child
  | Min
  | Max
  | Unlimited

module Grammar : Instance.S with type Node.t = grammar_node

val from_file :
  string ->
  ((module Instance.S),
   [> (Grammar.Node.t, Grammar.Node.t Position.t) parse_error]) Result.t
