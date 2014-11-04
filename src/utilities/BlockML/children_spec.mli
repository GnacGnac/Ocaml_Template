
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

  val nones : node t
end

module Make (M : M) : S with type node = M.t

type 'node env = ('node exp * int) list

type 'node occurrence_error = [
| `Children_spec_violation of ('node env * 'node t)
]

val check : 'node env -> 'node t -> (unit, [> 'node occurrence_error]) Result.t
