
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
     [> `Bad_int_occurrence of Node.t * int * Children_spec.Occurrence.t
      | `Bad_sub_node_occurrence of
	  Node.t * Node.t * int * Children_spec.Occurrence.t
      | `Bad_text_occurrence of Node.t * int * Children_spec.Occurrence.t
      | `File_does_not_exist of string
      | `Could_not_open_file of string
      | `Unrecognized_char of char * Position.t
      | `Parse_error of Position.t
      | `Not_a_root_node of Node.t option
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

  let analyze_children_spec name spec children =
    let add_occurrence (nb_ints, nb_texts, nb_sub_nodes) = function
      | Int _ -> (nb_ints + 1, nb_texts, nb_sub_nodes)
      | Text _ -> (nb_ints, nb_texts + 1, nb_sub_nodes)
      | Node (name, _) ->
	let old_occurrence = S.Children.NodeMap.find name nb_sub_nodes in
	let old_occurrence = match old_occurrence with
	  | Ok old_occurrence -> old_occurrence
	  | Error `Not_found -> 0 in
	let nb_sub_nodes =
	  S.Children.NodeMap.add name (old_occurrence + 1) nb_sub_nodes in
	(nb_ints, nb_texts, nb_sub_nodes) in
    let (nb_ints, nb_texts, nb_sub_nodes) =
      List.fold_left add_occurrence (0, 0, S.Children.NodeMap.empty) children in
    S.Children.check name spec nb_ints nb_texts nb_sub_nodes

  let rec analyze = function
    | Block_string.Int i -> return (int i)
    | Block_string.Text s -> return (text s)
    | Block_string.Node (name, children) -> analyze_block name children

  and analyze_block name children =
    analyze_name name >>= fun name ->
    analyze_children children >>= fun children ->
    analyze_children_spec name (S.spec name) children >>= fun () ->
    return (node name children)

  and analyze_name name =
    let f_error = function
      | `Unrecognized_string_assoc s -> `Unrecognized_node s in
    map_error f_error (S.of_string name)

  and analyze_children children = List_ext.bind analyze children

  let analyze_root = function
    | Int _ | Text _ -> error (`Not_a_root_node None)
    | Node (name, _) as block ->
      if S.Set.mem name S.possible_roots then return block
      else error (`Not_a_root_node (Some name))

  let parse file = Block_parse.from_file file >>= analyze >>= analyze_root

  let save file block =
    to_string block >>= Sys_ext.save file

end
