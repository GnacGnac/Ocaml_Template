
open Result


module type SPEC = sig
  include Stringable.S
  module Children : Children_spec.S with type node = t
  val spec : t -> Children.t
  val possible_roots : Set.t
end


module type S = sig
  include Block_generic.S
  val parse :
  string ->
    (t,
     [> `Bad_int_occurence of Node.t * int * Children_spec.Occurence.t
      | `Bad_sub_node_occurence of
	  Node.t * Node.t * int * Children_spec.Occurence.t
      | `Bad_text_occurence of Node.t * int * Children_spec.Occurence.t
      | `File_does_not_exist of string
      | `Could_not_open_file of string
      | `Unrecognized_char of char * Position.t
      | `Parse_error of Position.t
      | `Not_a_root_node of Node.t
      | `Unrecognized_node of string]) Result.t
  val save :
    string -> t ->
    (unit,
     [> `Could_not_save_in_file of string
      | `Node_not_bound_to_a_string of Node.t]) Result.t
end


module Make (Spec : SPEC) = struct

  module S = struct
    include Spec
    let to_string node =
      let f_error = function
	| `Unrecognized_elt_assoc elt -> `Node_not_bound_to_a_string elt in
      map_error f_error (to_string node)
  end

  include Block_generic.Make (S)

(*
  let analyze_children_spec block_name_opt spec children =
    let add_occurence (nb_ints, nb_texts, nb_sub_nodes) = function
      | Int _ -> (nb_ints + 1, nb_texts, nb_sub_nodes)
      | Text _ -> (nb_ints, nb_texts + 1, nb_sub_nodes)
      | Node (name, _) ->
	let old_occurence = S.Children.NodeMap.find name nb_sub_nodes in
	let old_occurence = match old_occurence with
	  | Ok old_occurence -> old_occurence
	  | Error `Not_found -> 0 in
	let nb_sub_nodes =
	  S.Children.NodeMap.add name (old_occurence + 1) nb_sub_nodes in
	(nb_ints + 1, nb_texts, nb_sub_nodes) in
    let (nb_ints, nb_texts, nb_sub_nodes) =
      List.fold_left add_occurence (0, 0, S.Children.NodeMap.empty) children in
    S.Children.check block_name spec nb_ints nb_texts nb_sub_nodes

  let rec analyze_block block =
    analyze_name (Block_string.name block) >>= fun block_name ->
    analyze_children (Block_string.children block) >>= fun children ->
    analyze_children_spec block_name (S.spec block_name) children >>= fun () ->
    return (node block_name children)

  and analyze_name name =
    let f_error = function
      | `Unrecognized_string_assoc s -> `Unrecognized_node s in
    map_error f_error (S.of_string name)

  and analyze_children children = List_ext.bind analyze_child children

  and analyze_child = function
    | Block_string.Int i -> return (int i)
    | Block_string.Text s -> return (text s)
    | Block_string.Block b -> analyze_block b >>= fun b -> return (block b)
*)

  let rec analyze_block = function
    | Block_string.Int i -> return (int i)
    | Block_string.Text s -> return (text s)
    | Block_string.Node (name, children) ->
      analyze_block b >>= fun b -> return (block b)

  let analyze block =
    analyze_block block >>= function
      | Block_string.Int _ | Block_string.Text _ -> error `Root_is_not_a_node
      | Block_string.Node (name, _) as block ->
	if S.Set.mem name S.possible_roots then return block
	else error (`Not_a_root_node name)

  let parse file =
    Parse.from_file file >>= fun block ->
    analyze block

  let save file block =
    to_string block >>= fun s ->
    Sys_ext.save file s

end
