
open Result


type unrecognized_string = [`Unrecognized_string of string]
type not_an_int = [`Not_an_int of string]


module type TO_STRING = sig
  type t
  val to_string : t -> string
end

module type OF_STRING = sig
  type t
  val of_string : string -> (t, unrecognized_string) Result.t
end

module type STRINGABLE = sig
  type t
  val to_string : t -> string
  val of_string : string -> (t, unrecognized_string) Result.t
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


let matched_indices_group nb_indices s =
  let rec aux acc i = if i <= 0 then acc else aux (i :: acc) (i - 1) in
  let res = aux [] nb_indices in
  let f i = int_of_string (Str.matched_group i s) in
  List.map f res

let sep = "_"

let parse_indices base nb_indices s =
  let index = sep ^ "\\([0-9]+\\)" in
  let indices = List_ext.make nb_indices index in
  let indices = List.fold_left (^) "" indices in
  let str = Str.regexp ("^" ^ base ^ indices ^ "$") in
  let success = Str.string_match str s 0 in
  if success then Some (matched_indices_group nb_indices s)
  else None

let add_indices base indices =
  let f res i = res ^ sep ^ (string_of_int i) in
  base ^ (List.fold_left f "" indices)

let to_int s =
  try return (int_of_string s)
  with Failure _ -> error (`Not_an_int s)

let uppercase_first s =
  let l = String.length s in
  if l = 0 then s
  else (String.uppercase (String.make 1 s.[0])) ^ (String.sub s 1 (l - 1))


let rec contains_rec s substring length substring_length i =
  if i + substring_length > length then None
  else
    if String.sub s i substring_length = substring then
      Some (i, i + substring_length)
    else contains_rec s substring length substring_length (i + 1)

let contains s ?(from=0) substring =
  contains_rec s substring (String.length s) (String.length substring) from
