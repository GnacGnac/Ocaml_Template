
open Result


module type SPEC = sig
  include String_ext.STRINGABLE
  module Set : Set_ext.S with type elt = t
  val spec : t -> t Children_spec.t
  val possible_roots : Set.t
end

type 'node occurrence_error = [
| `Children_spec_violation of
      ('node Position.t * 'node Children_spec.env * 'node Children_spec.t)
]

type 'node analyze_error = [
| 'node occurrence_error
| `Not_a_root_node of 'node option Position.t
]

type 'node parse_error = [
| 'node analyze_error
| Block_parse.error
| `Unrecognized_node of string Position.t
]

module type S = sig
  include Block_generic.S
  type node_analyze_error = Node.t analyze_error
  type node_parse_error = Node.t parse_error
  type could_not_write_to_file = [`Could_not_write_file of string]
  val analyze : t -> (unit, node_analyze_error) Result.t
  val parse : string -> (t, node_parse_error) Result.t
  val save : string -> t -> (unit, could_not_write_to_file) Result.t
end


module Make (Spec : SPEC) = struct

  include Block_generic.Make (Spec)

  type node_analyze_error = Node.t analyze_error
  type node_parse_error = Node.t parse_error
  type could_not_write_to_file = [`Could_not_write_file of string]

  let analyze_name block name =
    let f_error = function
      | `Unrecognized_string s ->
	`Unrecognized_node (Position.change_contents s block) in
    map_error f_error (Spec.of_string name)

  let primitive_content = function
    | Block_string.Int i -> int_content i
    | Block_string.Text s -> text_content s

  let rec analyze_names block = match Position.contents block with
    | Block_string.Primitive prim ->
      return (Position.change_contents (primitive_content prim) block)
    | Block_string.Node (name, children) ->
      analyze_name block name >>= fun name ->
      analyze_children_names children >>= fun children ->
      return (Position.change_contents (node_content name children) block)

  and analyze_children_names children = List_ext.bind analyze_names children

  let spec_primitive_of_primitive = function
    | Int _ -> Children_spec.Primitive (Children_spec.Int)
    | Text _ -> Children_spec.Primitive (Children_spec.Text)

  let exp_of_block block = match Position.contents block with
    | Primitive prim -> spec_primitive_of_primitive prim
    | Node (name, _) -> Children_spec.Var name

  let add_env env e =
    let f (found, env) (e', n) =
      let (found, added) = if e' = e then (true, 1) else (found, 0) in
      (found, (e', n + added) :: env) in
    let (found, env) = List.fold_left f (false, []) env in
    (if found then [] else [(e, 1)]) @ env

  let analyze_children_spec name spec children =
    let f_error = function
      | `Children_spec_violation (env, e) ->
	`Children_spec_violation (name, env, e) in
    let add_occurrence env child = add_env env (exp_of_block child) in
    let env = List.fold_left add_occurrence [] children in
    map_error f_error (Children_spec.check env spec)

  let rec analyze block = match Position.contents block with
    | Primitive _ -> return ()
    | Node (name, children) ->
      analyze_block (Position.change_contents name block) children

  and analyze_block name children =
    analyze_children children >>= fun () ->
    let contents = Position.contents name in
    analyze_children_spec name (Spec.spec contents) children >>= fun () ->
    return ()

  and analyze_children children =
    List_ext.fold_bind (fun () -> analyze) () children

  let analyze_root block = match Position.contents block with
    | Primitive _ ->
      error (`Not_a_root_node (Position.change_contents None block))
    | Node (name, _) ->
      if Spec.Set.mem name Spec.possible_roots then return ()
      else error (`Not_a_root_node (Position.change_contents (Some name) block))

  let analyze block =
    analyze_root block >>= fun () ->
    analyze block >>= fun () ->
    return ()

  let parse file =
    (Block_parse.from_file file :>
       (Block_string.t, node_parse_error) Result.t) >>=
    analyze_names >>= fun block ->
    analyze block >>= fun () ->
    return block

  let save file block = Sys_ext.write_file file (to_string block)

end


module type UNSAFE_SPEC = sig
  include String_ext.UNSAFE_STRINGABLE
  val spec : t -> t Children_spec.t
  val possible_roots : t list
end


module MakeUnsafe (Spec : UNSAFE_SPEC) = struct

  module FullSpec = struct

    module M = struct
      type t = Spec.t
      let compare = Pervasives.compare
    end

    type unrecognized_string = [`Unrecognized_string of string]

    include String_ext.MakeStringable (Spec)

    module Set = Set_ext.Make (M)

    let spec = Spec.spec

    let possible_roots =
      let f roots root = Set.add root roots in
      List.fold_left f Set.empty Spec.possible_roots

  end

  include Make (FullSpec)

end
