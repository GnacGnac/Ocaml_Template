
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

module Make (Spec : SPEC) : S with type Node.t = Spec.t
