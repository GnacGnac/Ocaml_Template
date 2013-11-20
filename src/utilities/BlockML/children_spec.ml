
open Result


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


let string_of_primitive = function
  | Int -> "int"
  | Text -> "text"

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"

let add_parenthesis_if_needed f e =
  let s = f e in
  match e with
  | Bin_op (_, _, _) -> "(" ^ s ^ ")"
  | _ -> s

let rec string_of_exp f = function
  | Cst i -> string_of_int i
  | Primitive primitive -> string_of_primitive primitive
  | Var v -> f v
  | Bin_op (bin_op, e1, e2) ->
    let bin_op = string_of_binop bin_op in
    let e1 = add_parenthesis_if_needed (string_of_exp f) e1 in
    let e2 = add_parenthesis_if_needed (string_of_exp f) e2 in
    e1 ^ " " ^ bin_op ^ " " ^ e2

let string_of_bin_cmp = function
  | Eq -> "="
  | Diff -> "<>"
  | Le -> "<="
  | Lt -> "<"
  | Ge -> ">="
  | Gt -> ">"

let string_of_un_con = function
  | Not -> "Not"

let string_of_bin_con = function
  | And -> "And"
  | Or -> "Or"

let rec to_string f = function
  | Bin_cmp (bin_cmp, e1, e2) ->
    (string_of_exp f e1) ^ " " ^ (string_of_bin_cmp bin_cmp) ^ " " ^
      (string_of_exp f e2)
  | Un_con (un_con, spec) ->
    (string_of_un_con un_con) ^ "(" ^ (to_string f spec) ^ ")"
  | True -> "True"
  | False -> "False"
  | Bin_con (bin_con, specs) ->
    let specs = List_ext.to_string " ; " (to_string f) specs in
    (string_of_bin_con bin_con) ^ "[" ^ specs ^ "]"


type 'a env = ('a exp * int) list

type 'a occurrence_error =
[ `Children_spec_violation of ('a env * 'a t) ]


let eval_bin_op = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> ( * )

let rec eval_exp env = function
  | e when List.mem_assoc e env -> List.assoc e env
  | Cst i -> i
  | Bin_op (bin_op, e1, e2) ->
    eval_bin_op bin_op (eval_exp env e1) (eval_exp env e2)
  | e -> 0

let eval_bin_cmp = function
  | Eq -> (=)
  | Diff -> (<>)
  | Le -> (<=)
  | Lt -> (<)
  | Ge -> (>=)
  | Gt -> (>)

let rec check env spec =
  let error = error (`Children_spec_violation (env, spec)) in
  match spec with
  | Bin_cmp (bin_cmp, e1, e2) -> check_bin_cmp error env bin_cmp e1 e2
  | Un_con (Not, spec') -> check_not error (check env spec')
  | Bin_con (And, specs) -> check_and (List.map (check env) specs)
  | Bin_con (Or, specs) -> check_or error (List.map (check env) specs)
  | True -> return ()
  | False -> error

and check_bin_cmp error env bin_cmp e1 e2 =
  if eval_bin_cmp bin_cmp (eval_exp env e1) (eval_exp env e2) then return ()
  else error

and check_not error = function
  | Ok _ -> error
  | Error _ -> return ()

and check_and specs = List_ext.fold_bind (fun () spec -> spec) () specs

and check_or error specs =
  let f res = function
    | Ok () -> return ()
    | Error _ -> res in
  List.fold_left f error specs


let rec base_exps_exp e = match e with
  | Cst _ -> []
  | Primitive _ | Var _ -> [e]
  | Bin_op (_, e1, e2) -> (base_exps_exp e1) @ (base_exps_exp e2)

let rec base_exps_rec = function
  | Bin_cmp (_, e1, e2) -> (base_exps_exp e1) @ (base_exps_exp e2)
  | Un_con (_, spec) -> base_exps_rec spec
  | True | False -> []
  | Bin_con (_, specs) -> List.flatten (List.map base_exps_rec specs)

let base_exps spec = List_ext.remove_doubles Pervasives.(=) (base_exps_rec spec)


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

module Make (M : M) = struct

  type node = M.t

  let cst n = Cst n
  let int = Primitive Int
  let text = Primitive Text
  let var a = Var a

  let bin_cmp bin_cmp a b = Bin_cmp (bin_cmp, a, b)
  let eq a b = bin_cmp Eq a b
  let le a b = bin_cmp Le a b

  let bin_con bin_con l = Bin_con (bin_con, l)
  let and_ l = bin_con And l
  let or_ l = bin_con Or l

  let nones_exp l = and_ (List.map (eq (cst 0)) l)
  let nones = nones_exp (List.map var M.all)

  let exp_large_interval e n m =
    let min = le (cst n) e in
    match m with
    | None -> min
    | Some m -> and_ [min ; le e (cst m)]

  let exact_exp n e = eq e (cst n)
  let exact n v = exact_exp n (var v)

  let one_exp e = exact_exp 1 e
  let one_int = one_exp int
  let one_text = one_exp text
  let one v = one_exp (Var v)
  let ones_exp exps = and_ (List.map one_exp exps)
  let ones vars = ones_exp (List.map var vars)

  let any_exp e = exp_large_interval e 0 None
  let any_int = any_exp int
  let any_text = any_exp text
  let any v = any_exp (Var v)
  let anys_exp exps = and_ (List.map any_exp exps)
  let anys vars = anys_exp (List.map var vars)

  let option_exp e = exp_large_interval e 0 (Some 1)
  let option_int = option_exp int
  let option_text = option_exp text
  let option v = option_exp (Var v)
  let options_exp exps = and_ (List.map option_exp exps)
  let options vars = options_exp (List.map var vars)

  let only spec =
    let bases = base_exps spec in
    let l = int :: text :: (List.map var M.all) in
    let l = List_ext.removes l bases in
    and_ [spec ; nones_exp l]

  let sum specs = or_ (List.map only specs)
  let sum_one_exp exps = sum (List.map one_exp exps)
  let sum_one vars = sum_one_exp (List.map var vars)

end
