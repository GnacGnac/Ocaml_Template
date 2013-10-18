
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
  | Bin_con (And, specs) -> check_and (List.map (check env) specs)
  | Bin_con (Or, specs) -> check_or error (List.map (check env) specs)
  | True -> return ()
  | False -> error

and check_bin_cmp error env bin_cmp e1 e2 =
  eval_exp env e1 >>= fun e1 ->
  eval_exp env e2 >>= fun e2 ->
  if eval_bin_cmp bin_cmp e1 e2 then return ()
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

let nones l = and_ (List.map (eq (cst 0)) l)

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

let only l spec =
  let bases = base_exps spec in
  let l = int :: text :: (List.map var l) in
  let l = List_ext.removes l bases in
  and_ [spec  ; nones l]

let sum l specs = or_ (List.map (only l) specs)
let sum_one_exp l exps = sum l (List.map one_exp exps)
let sum_one l vars = sum_one_exp l (List.map var vars)
