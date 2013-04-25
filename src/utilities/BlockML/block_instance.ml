
open Result


module type SPEC = sig
  type t
  module Set : Set_ext.S with type elt = t
  val to_string : t -> string
  val of_string : string -> (t, [> `Unrecognized_string of string]) Result.t
  module Children : Children_spec.S with type node = t
  val spec : t -> Children.t
  val possible_roots : Set.t
end


module type S = sig
  include Block_generic.S
  type node_pos = Node.t Position.t
  val analyze :
    t ->
    (unit,
     [> `Bad_int_occurrence of node_pos * int * Children_spec.Occurrence.t
      | `Bad_sub_node_occurrence of
	  node_pos * Node.t * int * Children_spec.Occurrence.t
      | `Bad_text_occurrence of node_pos * int * Children_spec.Occurrence.t
      | `Not_a_root_node of Node.t option Position.t]) Result.t       
  val parse :
  string ->
    (t,
     [> `Bad_int_occurrence of node_pos * int * Children_spec.Occurrence.t
      | `Bad_sub_node_occurrence of
	  node_pos * Node.t * int * Children_spec.Occurrence.t
      | `Bad_text_occurrence of node_pos * int * Children_spec.Occurrence.t
      | `File_does_not_exist of string
      | `Could_not_open_file of string
      | `Unrecognized_char of char Position.t
      | `Parse_error of unit Position.t
      | `Not_a_root_node of Node.t option Position.t
      | `Unrecognized_node of string Position.t]) Result.t
  val save :
    string -> t -> (unit, [> `Could_not_save_in_file of string]) Result.t
end


module Make (Spec : SPEC) = struct

  include Block_generic.Make (Spec)

  type node_pos = Node.t Position.t

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
    | Int _ -> Spec.Children.Int
    | Text _ -> Spec.Children.Text

  let update_nb_primitives primitive nb_primitives primitive' =
    (nb_primitives primitive) + (if primitive' = primitive then 1 else 0)

  let analyze_children_spec name spec children =
    let add_occurrence (nb_primitives, nb_sub_nodes) child =
      match Position.contents child with
	| Primitive prim ->
	  let spec_primitive = spec_primitive_of_primitive prim in
	  (update_nb_primitives spec_primitive nb_primitives, nb_sub_nodes)
	| Node (name, _) ->
	  let old_occurrence = Spec.Children.NodeMap.find name nb_sub_nodes in
	  let old_occurrence = match old_occurrence with
	    | Ok old_occurrence -> old_occurrence
	    | Error `Not_found -> 0 in
	  let nb_sub_nodes =
	    Spec.Children.NodeMap.add name (old_occurrence + 1) nb_sub_nodes in
	  (nb_primitives, nb_sub_nodes) in
    let (nb_primitives, nb_sub_nodes) =
      List.fold_left
	add_occurrence ((fun _ -> 0), Spec.Children.NodeMap.empty) children in
    let nb_children = Spec.Children.make nb_primitives nb_sub_nodes in
    Spec.Children.check name spec nb_children

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
    Block_parse.from_file file >>=
    analyze_names >>= fun block ->
    analyze block >>= fun () ->
    return block

  let save file block = Sys_ext.save file (to_string block)

end
