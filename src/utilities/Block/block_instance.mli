
module ChildrenSpec : sig
  type 'a t
end

module type SPEC = sig
  include Stringable.S
  val spec : t -> t ChildrenSpec.t
  val possible_roots : Set.t
end

module type S = sig
  include Block_generic.S
  val parse : string -> (t, [> `TODO]) Result.t
  val save : string -> t -> (unit, [> `TODO]) Result.t
end

module Make (Spec : SPEC) : S with type Node.t = Spec.t
