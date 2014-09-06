
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

module Make (Spec : SPEC) : S with type Node.t = Spec.t

module type UNSAFE_SPEC = sig
  include String_ext.UNSAFE_STRINGABLE
  val spec : t -> t Children_spec.t
  val possible_roots : t list
end

module MakeUnsafe (Spec : UNSAFE_SPEC) : S with type Node.t = Spec.t
