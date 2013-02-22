
open Result


module type NODE = sig
  type t
  val to_string : t -> string
end


module type S = sig
  module Node : NODE
  type contents = Int of int | Text of string | Node of Node.t * t list
  and t = contents Position.t
  val int_content : int -> contents
  val text_content : string -> contents
  val node_content : Node.t -> t list -> contents
  val int : int -> t
  val text : string -> t
  val node : Node.t -> t list -> t
  val to_string : t -> string
end


module Make (N : NODE) = struct

  module Node = N


  type contents = Int of int | Text of string | Node of Node.t * t list
  and t = contents Position.t

  let int_content i = Int i
  let text_content s = Text s
  let node_content name children = Node (name, children)

  let int i = Position.make_dummy (int_content i)
  let text s = Position.make_dummy (text_content s)
  let node name children = Position.make_dummy (node_content name children)


  let string_of_text s = "\"" ^ (String.escaped s) ^ "\""

  let string_of_position ?debug block = match debug with
    | None | Some false -> ""
    | Some true ->
(*
    | _ ->
*)
      let f_pos line char _ = Printf.sprintf "@(%d, %d)" line char in
      let f_no_pos _ = "" in
      Position.apply f_pos f_no_pos block

  let rec to_string space block = match Position.contents block with
    | Int i -> space ^ (string_of_int i) ^ (string_of_position block)
    | Text s -> space ^ (string_of_text s) ^ (string_of_position block)
    | Node (name, []) ->
      space ^ (Node.to_string name) ^ (string_of_position block)
    | Node (name, [child]) -> to_string_one_child block space name child
    | Node (name, children) ->
      let children = string_of_children (space ^ "  ") children in
      let name = Node.to_string name in
      space ^ name ^ (string_of_position block) ^
	" {\n" ^ children ^ "\n" ^ space ^ "}"

  and to_string_one_child block space name child =
    match Position.contents child with
      | Int i ->
	space ^ (Node.to_string name) ^ (string_of_position block) ^
	  " { " ^ (string_of_int i) ^ " }"
      | Text s ->
	space ^ (Node.to_string name) ^ (string_of_position block) ^
	  " { " ^ (string_of_text s) ^ " }"
      | Node _ ->
	let child = to_string (space ^ "  ") child in
	let name = Node.to_string name in
	space ^ name ^ (string_of_position block) ^
	  " {\n" ^ child ^ "\n" ^ space ^ "}"

  and string_of_children space children =
    List_ext.to_string "\n" (to_string space) children

  let to_string = to_string ""

end
