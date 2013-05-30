
open Result


module type TO_STRING = sig
  type t
  val to_string : t -> string
end

module type OF_STRING = sig
  type t
  val of_string : string -> (t, [> `Unrecognized_string of string]) Result.t
end

module type STRINGABLE = sig
  type t
  val to_string : t -> string
  val of_string : string -> (t, [> `Unrecognized_string of string]) Result.t
end

module type UNSAFE_STRINGABLE = sig
  type t
  val string_assoc : (t * string) list
end


module MakeStringable (UnsafeStringable : UNSAFE_STRINGABLE) = struct
  type t = UnsafeStringable.t
  let string_assoc =
    List.map (fun (x, y) -> (x, String.lowercase y))
      UnsafeStringable.string_assoc
  let assoc_string = List.map (fun (x, y) -> (y, x)) string_assoc
  let to_string node = match List_ext.assoc node string_assoc with
    | Ok s -> s
    | Error _ ->
      (* Should not happen. If so, check that every node is associated a
	 string in [UnsafeStringable.string_assoc]. *)
      assert false
  let of_string s =
    map_error (fun `Not_found -> `Unrecognized_string s)
      (List_ext.assoc (String.lowercase s) assoc_string)
end


module Set = Set_ext.Make (String)

module Map = Map_ext.Make (String)


let repeat n s = List.fold_left (^) "" (List_ext.repeat n s)


let remove_chars s cl =
  let l = String.length s in
  let rec aux i =
    if i >= l then ""
    else
      if List.mem s.[i] cl then aux (i+1)
      else (String.make 1 s.[i]) ^ (aux (i+1)) in
  aux 0
