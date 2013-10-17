
open Result


type primitive = Int | Text

type bin_op = Add | Sub | Mul

type 'a exp =
| Primitive of primitive
| Var of 'a
| Cst of int
| Bin_op of bin_op * 'a exp * 'a exp

type bin_cmp = Eq | Diff | Le | Lt | Ge | Gt

type un_con = Not
type bin_con = And | Or

type 'a t =
| Bin_cmp of bin_cmp * 'a exp * 'a exp
| Un_con of un_con * 'a t
| Bin_con of bin_con * 'a t * 'a t


type 'a env = ('a exp * int) list

type 'a occurrence_error =
[ `Unknown_children_spec_expression of ('a env * 'a exp)
| `Children_spec_violation of ('a env * 'a t) ]


let eval_bin_op = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> ( * )

let rec eval_exp env = function
  | e when List.mem_assoc e env -> return (List.assoc e env)
  | Cst i -> return i
  | Bin_op (bin_op, e1, e2) ->
    eval_exp env e1 >>= fun e1 ->
    eval_exp env e2 >>= fun e2 ->
    return (eval_bin_op bin_op e1 e2)
  | e -> error (`Unknown_children_spec_expression (env, e))

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
  | Bin_con (And, spec1, spec2) -> check_and (check env spec1) (check env spec2)
  | Bin_con (Or, spec1, spec2) ->
    check_or error (check env spec1) (check env spec2)

and check_bin_cmp error env bin_cmp e1 e2 =
  eval_exp env e1 >>= fun e1 ->
  eval_exp env e2 >>= fun e2 ->
  if eval_bin_cmp bin_cmp e1 e2 then return ()
  else error

and check_not error = function
  | Ok _ -> error
  | Error _ -> return ()

and check_and spec1 spec2 =
  spec1 >>= fun () ->
  spec2 >>= fun () ->
  return ()

and check_or error spec1 spec2 = match spec1, spec2 with
  | Ok (), _ | _, Ok () -> return ()
  | Error _, Error _ -> error
