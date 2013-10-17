
module Generic : sig

  module type NODE = sig
    type t
    val to_string : t -> string
  end

  module type S = sig
    module Node : NODE
    type primitive = Int of int | Text of string
    type contents = Primitive of primitive | Node of Node.t * t list
    and t = contents Position.t
    val int_content : int -> contents
    val text_content : string -> contents
    val node_content : Node.t -> t list -> contents
    val int : int -> t
    val text : string -> t
    val node : Node.t -> t list -> t
    val get_int : t -> (int, [> `No_int]) Result.t
    val get_int_with_pos : t -> (int Position.t, [> `No_int]) Result.t
    val get_text : t -> (string, [> `No_text]) Result.t
    val get_text_with_pos : t -> (string Position.t, [> `No_text]) Result.t
    val get_node : Node.t -> t -> (t, [> `No_such_child]) Result.t
    val get_node_no_pos : Node.t -> t -> (contents, [> `No_such_child]) Result.t
    val get_int_children : t -> (int list, [> `No_children]) Result.t
    val get_int_children_with_pos :
      t -> (int Position.t list, [> `No_children]) Result.t
    val get_text_children : t -> (string list, [> `No_children]) Result.t
    val get_text_children_with_pos :
      t -> (string Position.t list, [> `No_children]) Result.t
    val get_node_children :
      Node.t -> t -> (t list, [> `No_children]) Result.t
    val get_node_children_no_pos :
      Node.t -> t -> (contents list, [> `No_children]) Result.t
    val get_root_node : t -> (Node.t, [> `Not_a_node]) Result.t
    val get_root_node_with_pos :
      t -> (Node.t Position.t, [> `Not_a_node]) Result.t
    val get_children : t -> (t list, [> `No_children]) Result.t
    val get_children_no_pos : t -> (contents list, [> `No_children]) Result.t
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

end

(*
module Occurrence : sig
  type bound = Int of int | Infty
  type t
  val make : int -> bound -> t
  val any : t
  val anys : 'a list -> ('a * t) list
  val none : t
  val exactly : int -> t
  val exactlys : int -> 'a list -> ('a * t) list
  val at_least : int -> t
  val at_leasts : int -> 'a list -> ('a * t) list
  val at_most : int -> t
  val at_mosts : int -> 'a list -> ('a * t) list
  val between : int -> int -> t
  val betweens : int -> int -> 'a list -> ('a * t) list
  val option : t
  val options : 'a list -> ('a * t) list
  val one : t
  val ones : 'a list -> ('a * t) list
  val to_string : t -> string
end

module Primitive : sig
  type t = Int | Text
  type 'a specification = t -> 'a
  type occurrence_specification = Occurrence.t specification
  val none : occurrence_specification
  val anys : occurrence_specification
  val one : t -> occurrence_specification
  val any : t -> occurrence_specification
  val option : t -> occurrence_specification
  val one_int : occurrence_specification
  val one_text : occurrence_specification
  val any_int : occurrence_specification
  val any_text : occurrence_specification
  val option_int : occurrence_specification
  val option_text : occurrence_specification
end

type 'node occurrence_error =
  [ `Bad_int_occurrence of 'node Position.t * int * Occurrence.t
  | `Bad_text_occurrence of 'node Position.t * int * Occurrence.t
  | `Bad_sub_node_occurrence of 'node Position.t * 'node * int * Occurrence.t]
*)

module ChildrenSpec : sig

  type primitive = Int | Text
  type bin_op = Add | Sub | Mul
  type 'node exp =
  | Primitive of primitive
  | Var of 'node
  | Cst of int
  | Bin_op of bin_op * 'node exp * 'node exp
  type bin_cmp = Eq | Diff | Le | Lt | Ge | Gt
  type un_con = Not
  type bin_con = And | Or
  type 'node t =
  | Bin_cmp of bin_cmp * 'node exp * 'node exp
  | Un_con of un_con * 'node t
  | Bin_con of bin_con * 'node t * 'node t
  type 'node env = ('node exp * int) list

  type 'node occurrence_error =
  [ `Unknown_children_spec_expression of ('node env * 'node exp)
  | `Children_spec_violation of ('node env * 'node t) ]

  val check :
    'node env -> 'node t -> (unit, [> 'node occurrence_error]) Result.t

(*
  type 'a unsafe_children_specification = ('a * Occurrence.t) list

  val empty : 'a unsafe_children_specification
  val all : Occurrence.t -> 'a list -> 'a unsafe_children_specification
  val any : 'a -> 'a unsafe_children_specification
  val one : 'a -> 'a unsafe_children_specification
  val option : 'a -> 'a unsafe_children_specification
  val anys : 'a list -> 'a unsafe_children_specification
  val ones : 'a list -> 'a unsafe_children_specification
  val options : 'a list -> 'a unsafe_children_specification

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

  module Make (Node : Map_ext.ORDERED_TYPE) : S with type node = Node.t
*)

end

type 'node occurrence_error =
[ `Unknown_children_spec_expression of
    ('node Position.t * 'node ChildrenSpec.env * 'node ChildrenSpec.exp)
| `Children_spec_violation of
    ('node Position.t * 'node ChildrenSpec.env * 'node ChildrenSpec.t) ]

type 'node analyze_error =
  [ 'node occurrence_error
  | `Not_a_root_node of 'node option Position.t ]

type 'node parse_error =
  [ 'node analyze_error
  | `File_does_not_exist of string
  | `Could_not_open_file of string
  | `Unrecognized_char of char Position.t
  | `Unterminated_comment of unit Position.t
  | `Parse_error of unit Position.t
  | `Unrecognized_node of string Position.t ]

type 'node grammar_parse_error =
  [ 'node parse_error
  | `Grammar_unrecognized_node of string Position.t ]

module Instance : sig

  module type SPEC = sig
    include String_ext.STRINGABLE
    module Set : Set_ext.S with type elt = t
    val spec : t -> t ChildrenSpec.t
    val possible_roots : Set.t
  end

  module type S = sig
    include Generic.S
    val analyze : t -> (unit, [> Node.t analyze_error]) Result.t
    val parse : string -> (t, [> Node.t parse_error]) Result.t
    val save :
      string -> t -> (unit, [> `Could_not_write_file of string]) Result.t
  end

  module Make (Spec : SPEC) : S with type Node.t = Spec.t

  module type UNSAFE_SPEC = sig
    include String_ext.UNSAFE_STRINGABLE
    val spec : t -> t ChildrenSpec.t
    val possible_roots : t list
  end

  module MakeUnsafe (Spec : UNSAFE_SPEC) : S with type Node.t = Spec.t

end


(*
module Grammar : sig
  type node =
    | Grammar
    | Possible_roots
    | Children_specs
    | Children_spec
    | Name
    | Int
    | Text
    | Children
    | Child
    | Cardinality
    | Min
    | Max
  include Instance.S with type Node.t = node
  module type S = sig
    type t
    module type S = Instance.S with type Node.t = t
    val from_file :
      string ->
      ((module S), [> node grammar_parse_error]) Result.t
  end
  module type M = sig
    include String_ext.STRINGABLE
    val compare : t -> t -> int
  end
  module Make (M : M) : S with type t = M.t
  module MakeUnsafe (M : String_ext.UNSAFE_STRINGABLE) : S with type t = M.t
end


module type PARSE_RESULT = sig
  include Instance.S
  val parse_result : (t, [> Node.t parse_error]) Result.t
end

val parse_from_external :
  (module String_ext.UNSAFE_STRINGABLE) -> string -> string ->
  ((module PARSE_RESULT),
   [> `Grammar_error of [> Grammar.node grammar_parse_error]]) Result.t
*)
