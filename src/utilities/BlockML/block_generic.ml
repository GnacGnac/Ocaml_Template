
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
  val get_int : t list -> (int Position.t, [> `No_int]) Result.t
  val get_text : t list -> (string Position.t, [> `No_text]) Result.t
  val get_node :
    Node.t -> t list -> (t list Position.t, [> `No_such_child]) Result.t
  val to_string : t -> string

  (* Unsafe functions: raises assertion failure. *)
  val extract_get : (t list -> ('a Position.t, 'b) Result.t) -> t list -> 'a
  val extract_int : t list -> int
  val extract_text : t list -> string
  val extract_node : Node.t -> t list -> t list
  val extract_child_node : Node.t -> t -> t list
  val extract_int1 : t -> int
  val extract_text1 : t -> string
  val extract_children : t -> t list
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

  let get f res_error l =
    let f' res a = match res with
      | Ok _ -> res
      | Error `Not_found ->
	let res = f (Position.contents a) in
	map_result (fun res -> Position.change_contents res a) res in
    let f_error = function `Not_found -> res_error in
    map_error f_error (List.fold_left f' (error `Not_found) l)

  let get_int =
    get (function Int i -> return i | _ -> error `Not_found) `No_int

  let get_text =
    get (function Text s -> return s | _ -> error `Not_found) `No_text

  let get_children =
    get
      (function Node (_, children) -> return children | _ -> error `Not_found)
      `No_children

  let get_node node =
    let f = function
      | Node (node', children) when node' = node ->
	return children
      | _ -> error `Not_found in
    get f `No_such_child

  let extract_get get e = Position.contents (extract (get e))
  let extract_int = extract_get get_int
  let extract_text = extract_get get_text
  let extract_node node = extract_get (get_node node)
  let extract_children node = extract_get get_children [node]
  let extract_child_node node block = extract_node node (extract_children block)
  let extract_int1 node = extract_int [node]
  let extract_text1 node = extract_text [node]


  let escaped s =
    let to_escape = ['"' ; '\\'] in
    let length = String.length s in
    let rec aux acc i =
      if i >= length then acc
      else
	let added =
	  if List.mem s.[i] to_escape then "\\"
	  else "" in
	aux (acc ^ added ^ (String.make 1 s.[i])) (i + 1) in
    aux "" 0

  let string_of_text s = "\"" ^ (escaped s) ^ "\""

  let string_of_position ?debug block = match debug with
    | None | Some false -> ""
    | Some true ->
      let f_pos line char _ = Printf.sprintf "@(%d, %d)" line char in
      let f_no_pos _ = "" in
      Position.apply f_pos f_no_pos block

  let string_of_position block = string_of_position ~debug:false block

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
