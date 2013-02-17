
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
  val parse : string -> (t, [> `TODO]) Result.t
  val save : string -> t -> (unit, [> `TODO]) Result.t
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

  let parse file = assert false (* TODO *)

  let save file block = assert false (* TODO *)

end
