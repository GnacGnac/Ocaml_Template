
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
  val node_string : (t * string) list
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
