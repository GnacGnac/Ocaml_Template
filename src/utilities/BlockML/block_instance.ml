
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
  val parse :
  string ->
    (t,
     [> `Bad_int_occurrence of Node.t * int * Children_spec.Occurrence.t
      | `Bad_sub_node_occurrence of
	  Node.t * Node.t * int * Children_spec.Occurrence.t
      | `Bad_text_occurrence of Node.t * int * Children_spec.Occurrence.t
      | `File_does_not_exist of string
      | `Could_not_open_file of string
      | `Unrecognized_char of char Position.t
      | `Parse_error of unit Position.t
      | `Not_a_root_node of Node.t option
      | `Unrecognized_node of string]) Result.t
  val save :
    string -> t -> (unit, [> `Could_not_save_in_file of string]) Result.t
end


module Make (Spec : SPEC) = struct

  include Block_generic.Make (Spec)

  let analyze_children_spec name spec children =
    let add_occurrence (nb_ints, nb_texts, nb_sub_nodes) child =
      match Position.contents child with
	| Int _ -> (nb_ints + 1, nb_texts, nb_sub_nodes)
	| Text _ -> (nb_ints, nb_texts + 1, nb_sub_nodes)
	| Node (name, _) ->
	  let old_occurrence = Spec.Children.NodeMap.find name nb_sub_nodes in
	  let old_occurrence = match old_occurrence with
	    | Ok old_occurrence -> old_occurrence
	    | Error `Not_found -> 0 in
	  let nb_sub_nodes =
	    Spec.Children.NodeMap.add name (old_occurrence + 1) nb_sub_nodes in
	  (nb_ints, nb_texts, nb_sub_nodes) in
    let (nb_ints, nb_texts, nb_sub_nodes) =
      List.fold_left
	add_occurrence (0, 0, Spec.Children.NodeMap.empty) children in
    Spec.Children.check name spec nb_ints nb_texts nb_sub_nodes

  let rec analyze block = match Position.contents block with
    | Block_string.Int i ->
      return (Position.change_contents (int_content i) block)
    | Block_string.Text s ->
      return (Position.change_contents (text_content s) block)
    | Block_string.Node (name, children) -> analyze_block block name children

  and analyze_block block name children =
    analyze_name name >>= fun name ->
    analyze_children children >>= fun children ->
    analyze_children_spec name (Spec.spec name) children >>= fun () ->
    return (Position.change_contents (node_content name children) block)

  and analyze_name name =
    let f_error = function `Unrecognized_string s -> `Unrecognized_node s in
    map_error f_error (Spec.of_string name)

  and analyze_children children = List_ext.bind analyze children

  let analyze_root block = match Position.contents block with
    | Int _ | Text _ -> error (`Not_a_root_node None)
    | Node (name, _) ->
      if Spec.Set.mem name Spec.possible_roots then return block
      else error (`Not_a_root_node (Some name))

  let parse file = Block_parse.from_file file >>= analyze >>= analyze_root

  let save file block = Sys_ext.save file (to_string block)

end
