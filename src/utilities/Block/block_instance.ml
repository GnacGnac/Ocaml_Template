
open Result


module ChildrenSpec = struct

  type 'a t = unit (* TODO *)

end


module type SPEC = sig
  include Stringable.S
  val spec : t -> t ChildrenSpec.t
  val possible_roots : Set.t
end


module type S = sig
  include Block_generic.S
  val parse :
  string ->
    (t,
     [> `File_does_not_exist of string
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

  let analyze_children_spec spec children =
    return () (* TODO *)

  let rec analyze_block block =
    analyze_name (Block_string.name block) >>= fun block_name ->
    analyze_children (Block_string.children block) >>= fun children ->
    analyze_children_spec (S.spec block_name) children >>= fun () ->
    return (node block_name children)

  and analyze_name name =
    let f_error = function
      | `Unrecognized_string_assoc s -> `Unrecognized_node s in
    map_error f_error (S.of_string name)

  and analyze_children children = List_ext.bind analyze_child children

  and analyze_child child = assert false (* TODO *)

  let analyze block =
    analyze_block block >>= fun block ->
    let name = name block in
    if S.Set.mem name S.possible_roots then return block
    else error (`Not_a_root_node name)

  let parse file =
    Parse.from_file file >>= fun block ->
    analyze block

  let save file block =
    to_string block >>= fun s ->
    Sys_ext.save file s

end
