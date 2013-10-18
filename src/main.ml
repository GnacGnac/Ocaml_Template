
open Amount
open Bijection
open Error
open Html
open Http
open List_ext
open Map_ext
open Option
open Position
open Result
open Set_ext
open String_ext
open Sys_ext
open BlockML
open Debug
(* open Grammar_example *)
open Array_ext


open Result


type node = Term | Var | App | Abs

module M = struct
  type t = node
  let string_assoc =
    [(Term, "term") ; (Var, "var") ; (App, "app") ; (Abs, "abs")]
  let all = List.map fst string_assoc
end


let string_of_primitive = function
  | BlockML.ChildrenSpec.Int -> "int"
  | BlockML.ChildrenSpec.Text -> "text"

let string_of_exp f = function
  | BlockML.ChildrenSpec.Cst i -> string_of_int i
  | BlockML.ChildrenSpec.Primitive primitive -> string_of_primitive primitive
  | BlockML.ChildrenSpec.Var node -> f node
  | BlockML.ChildrenSpec.Bin_op _ -> "binop"

let string_of_env f env =
  "" (* TODO *)

let string_of_pos a = match Position.all a with
  | Ok (file, line, char) ->
    Printf.sprintf "in file %s, line %d, character %d, " file line char
  | Error `No_position -> ""

let string_of_error f = function
  | `Parse_error pos -> (string_of_pos pos) ^ "parse error."
(*
  | `Bad_sub_node_occurrence (parent, child, i, occ) ->
    Printf.sprintf "%sbad child occurrence %s for %s (%d not in %s)."
      (string_of_pos parent) (f child) (f (Position.contents parent)) i
      (BlockML.Occurrence.to_string occ)
*)
  | `Not_a_root_node node_opt ->
    Printf.sprintf "%s%s is not a root node."
      (string_of_pos node_opt)
      (match Position.contents node_opt with
	| None -> "primitive node."
	| Some node -> f node)
  | `Unrecognized_char c ->
    Printf.sprintf "%sunrecognized character `%c`."
      (string_of_pos c) (Position.contents c)
  | `Unterminated_comment pos ->
    Printf.sprintf "%sunterminated comment." (string_of_pos pos)
  | `File_does_not_exist file -> "file " ^ file ^ " does not exist."
  | `Could_not_open_file file -> "could not open file " ^ file ^ "."
(*
  | `Bad_int_occurrence (node, i, occ) ->
    Printf.sprintf "%sbad int children occurrence for %s (%d not in %s)."
      (string_of_pos node) (f (Position.contents node)) i
      (BlockML.Occurrence.to_string occ)
  | `Bad_text_occurrence (node, i, occ) ->
    Printf.sprintf "%sbad text children occurrence for %s (%d not in %s)."
      (string_of_pos node) (f (Position.contents node)) i
      (BlockML.Occurrence.to_string occ)
*)
  | `Unrecognized_node s ->
    Printf.sprintf "%sunrecognized node %s."
      (string_of_pos s) (Position.contents s)
  | `Grammar_unrecognized_node s ->
    Printf.sprintf "%s%s is not a node of the grammar."
      (string_of_pos s) (Position.contents s)
  | `Children_spec_violation (name, env, spec) ->
    Printf.sprintf "%s%s." (string_of_pos name) (f (Position.contents name))

let show_error f error = Error.show (string_of_error f error)


module Spec = struct

  module S = BlockML.ChildrenSpec

  type t = node

  let string_assoc = M.string_assoc

  let only = S.only M.all
  let sum_one = S.sum_one M.all

  let spec = function
    | Term -> sum_one [Var ; Abs ; App]
    | Var -> only S.one_text
    | Abs -> only (S.ones_exp [S.text ; S.var Term])
    | App -> only (S.exact 2 Term)

  let possible_roots = [Term]

end

module Block = BlockML.Instance.MakeUnsafe (Spec)


let () =
  if Array.length Sys.argv >= 2 then
    let file = Sys.argv.(1) in
    match Block.parse file with
    | Ok block -> Printf.printf "%s\n%!" (Block.to_string block)
    | Error error -> show_error Block.Node.to_string error
  else Error.show ("usage: " ^ Sys.argv.(0) ^ " file")
