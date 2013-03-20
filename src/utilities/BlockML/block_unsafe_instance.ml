
open Result


module type SPEC = sig
  type t
  val node_string : (t * string) list
  module Children : Children_spec.S with type node = t
  val spec : t -> Children.t
  val possible_roots : t list
end


module Make (Spec : SPEC) = struct

  module FullSpec = struct

    module M = struct
      type t = Spec.t
      let compare = Pervasives.compare
    end

    include M

    module Set = Set_ext.Make (M)

    let node_string =
      List.map (fun (x, y) -> (x, String.lowercase y)) Spec.node_string

    let string_node = List.map (fun (x, y) -> (y, x)) node_string

    let to_string node =
      try List.assoc node node_string
      with Not_found ->
	(* Should not happen. If so, check that every node is associated a
	   string in [node_string]. *)
	assert false

    let of_string s =
      try return (List.assoc (String.lowercase s) string_node)
      with Not_found -> error (`Unrecognized_string s)

    module Children = Spec.Children

    let spec = Spec.spec

    let possible_roots =
      let f roots root = Set.add root roots in
      List.fold_left f Set.empty Spec.possible_roots

  end

  include Block_instance.Make (FullSpec)

end
