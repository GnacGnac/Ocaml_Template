
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

module ChildrenSpec : sig

  type primitive = Int | Text
  type bin_op = Add | Sub | Mul
  type 'a exp =
  | Cst of int
  | Primitive of primitive
  | Var of 'a
  | Bin_op of bin_op * 'a exp * 'a exp
  type bin_cmp = Eq | Diff | Le | Lt | Ge | Gt
  type un_con = Not
  type bin_con = And | Or
  type 'a t =
  | Bin_cmp of bin_cmp * 'a exp * 'a exp
  | Un_con of un_con * 'a t
  | True
  | False
  | Bin_con of bin_con * 'a t list

  val string_of_exp : ('node -> string) -> 'node exp -> string
  val to_string : ('node -> string) -> 'node t -> string

  module type M = sig type t val all : t list end

  module type S = sig
    type node

    val cst : int -> node exp
    val int : node exp
    val text : node exp
    val var : node -> node exp

    val eq : node exp -> node exp -> node t
    val and_ : node t list -> node t

    val exact : int -> node -> node t

    val one_int : node t
    val one_text : node t
    val one : node -> node t
    val one_exp : node exp -> node t
    val ones : node list -> node t
    val ones_exp : node exp list -> node t

    val any_int : node t
    val any_text : node t
    val any : node -> node t
    val any_exp : node exp -> node t
    val anys : node list -> node t
    val anys_exp : node exp list -> node t

    val option_int : node t
    val option_text : node t
    val option : node -> node t
    val option_exp : node exp -> node t
    val options : node list -> node t
    val options_exp : node exp list -> node t

    val sum : node t list -> node t
    val sum_one : node list -> node t
    val sum_one_exp : node exp list -> node t

    val only : node t -> node t
  end

  module Make (M : M) : S with type node = M.t

  type 'node env = ('node exp * int) list

  type 'node occurrence_error =
  [ `Children_spec_violation of ('node env * 'node t) ]

  val check :
    'node env -> 'node t -> (unit, [> 'node occurrence_error]) Result.t

end

type 'node occurrence_error =
[ `Children_spec_violation of
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
['node parse_error | `Grammar_unrecognized_node of string Position.t]

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


module Grammar : sig
  type node =
    | Grammar
    | Possible_roots
    | Children_specs
    | Children_spec
    | Node
    | Spec
    | Bin_cmp
    | Un_con
    | Bin_con
    | True
    | False
    | And
    | Or
    | Not
    | Eq
    | Diff
    | Le
    | Lt
    | Ge
    | Gt
    | Exp
    | Cst
    | Primitive
    | Var
    | Bin_op
    | Add
    | Sub
    | Mul
    | Int
    | Text
  module type S = sig
    type t
    module type S = Instance.S with type Node.t = t
    val from_file :
      string -> ((module S), [> node grammar_parse_error]) Result.t
  end
  include Instance.S with type Node.t = node
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
