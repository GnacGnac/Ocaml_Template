
module Generic = Block_generic
module Occurrence = Children_spec.Occurrence
module ChildrenSpec = Children_spec
module Instance = Block_instance
module UnsafeInstance = Block_unsafe_instance


module type S = sig

  module type SPEC = sig
    type t
  end

  module type S = sig
    include Generic.S
    type node_pos = Node.t Position.t
    val analyze :
      t ->
      (unit,
       [> `Bad_int_occurrence of node_pos * int * Occurrence.t
        | `Bad_sub_node_occurrence of node_pos * Node.t * int * Occurrence.t
        | `Bad_text_occurrence of node_pos * int * Occurrence.t
        | `Not_a_root_node of Node.t option Position.t]) Result.t
    val parse :
    string ->
      (t,
       [> `Bad_int_occurrence of node_pos * int * Occurrence.t
        | `Bad_sub_node_occurrence of node_pos * Node.t * int * Occurrence.t
        | `Bad_text_occurrence of node_pos * int * Occurrence.t
        | `File_does_not_exist of string
        | `Could_not_open_file of string
        | `Unrecognized_char of char Position.t
        | `Parse_error of unit Position.t
        | `Not_a_root_node of Node.t option Position.t
        | `Unrecognized_node of string Position.t]) Result.t
    val save :
      string -> t -> (unit, [> `Could_not_save_in_file of string]) Result.t
  end

  module Make (Spec : SPEC) : S with type Node.t = Spec.t

end
