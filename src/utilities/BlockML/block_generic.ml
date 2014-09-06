
open Result


module type NODE = sig
  type t
  val to_string : t -> string
end


module type S = sig
  module Node : NODE
  type primitive = Int of int | Text of string
  type contents = Primitive of primitive | Node of Node.t * t list
  and t = contents Position.t

  type no_int = [`No_int]
  type no_text = [`No_text]
  type no_such_child = [`No_such_child]
  type no_children = [`No_children]
  type not_a_node = [`Not_a_node]

  val int_content : int -> contents
  val text_content : string -> contents
  val node_content : Node.t -> t list -> contents
  val int : int -> t
  val text : string -> t
  val node : Node.t -> t list -> t
  val get_int : t -> (int, no_int) Result.t
  val get_int_with_pos : t -> (int Position.t, no_int) Result.t
  val get_text : t -> (string, no_text) Result.t
  val get_text_with_pos : t -> (string Position.t, no_text) Result.t
  val get_node : Node.t -> t -> (t, no_such_child) Result.t
  val get_node_no_pos : Node.t -> t -> (contents, no_such_child) Result.t
  val get_int_children : t -> (int list, no_children) Result.t
  val get_int_children_with_pos :
    t -> (int Position.t list, no_children) Result.t
  val get_text_children : t -> (string list, no_children) Result.t
  val get_text_children_with_pos :
    t -> (string Position.t list, no_children) Result.t
  val get_node_children : Node.t -> t -> (t list, no_children) Result.t
  val get_node_children_no_pos :
    Node.t -> t -> (contents list, no_children) Result.t
  val get_root_node : t -> (Node.t, not_a_node) Result.t
  val get_root_node_with_pos :
    t -> (Node.t Position.t, not_a_node) Result.t
  val get_children : t -> (t list, no_children) Result.t
  val get_children_no_pos : t -> (contents list, no_children) Result.t

  val to_string : t -> string

  (* Unsafe functions: raise assertion failure. *)
  val extract_int : t -> int
  val extract_int_with_pos : t -> int Position.t
  val extract_text : t -> string
  val extract_text_with_pos : t -> string Position.t
  val extract_node : Node.t -> t -> t
  val extract_node_no_pos : Node.t -> t -> contents
  val extract_int_children : t -> int list
  val extract_int_children_with_pos : t -> int Position.t list
  val extract_text_children : t -> string list
  val extract_text_children_with_pos : t -> string Position.t list
  val extract_node_children : Node.t -> t -> t list
  val extract_node_children_no_pos : Node.t -> t -> contents list
  val extract_root_node : t -> Node.t
  val extract_root_node_with_pos : t -> Node.t Position.t
  val extract_children : t -> t list
  val extract_children_no_pos : t -> contents list
end


module Make (N : NODE) = struct

  module Node = N


  type no_int = [`No_int]
  type no_text = [`No_text]
  type no_such_child = [`No_such_child]
  type no_children = [`No_children]
  type not_a_node = [`Not_a_node]


  type primitive = Int of int | Text of string
  type contents = Primitive of primitive | Node of Node.t * t list
  and t = contents Position.t

  let int_content i = Primitive (Int i)
  let text_content s = Primitive (Text s)
  let node_content name children = Node (name, children)

  let int i = Position.make_dummy (int_content i)
  let text s = Position.make_dummy (text_content s)
  let node name children = Position.make_dummy (node_content name children)

  let get_node_component block = match Position.contents block with
    | Node (node, children) ->
      return (Position.change_contents node block, children)
    | _ -> error `Not_a_node

  let get_root_node_with_pos block =
    get_node_component block >>= fun (node, _) ->
    return node

  let get_children block =
    map_error (function `Not_a_node -> `No_children)
      (get_node_component block >>= fun (_, children) ->
       return children)

  let get_children_from_children f block =
    let f' res a =
      res @
	(match f (Position.contents a) with
	| None -> []
	| Some e -> [Position.change_contents e a]) in
    get_children block >>= fun children ->
    return (List.fold_left f' [] children)

  let get_int_children_with_pos =
    get_children_from_children
      (function Primitive (Int i) -> Some i | _ -> None)

  let get_text_children_with_pos =
    get_children_from_children
      (function Primitive (Text s) -> Some s | _ -> None)

  let get_node_children node =
    get_children_from_children
      (function
	| Node (node', _) as block when node' = node -> Some block
	| _ -> None)

  let get_child_from_children f res_error block =
    map_error (fun _ -> res_error) (f block) >>= function
    | [] -> error res_error
    | e :: _ -> return e

  let get_int_with_pos =
    get_child_from_children get_int_children_with_pos `No_int

  let get_text_with_pos =
    get_child_from_children get_text_children_with_pos `No_text

  let get_node node =
    get_child_from_children (get_node_children node) `No_such_child

  let get_children_from_pos f block =
    f block >>= fun children ->
    return (List.map Position.contents children)

  let get_int_children = get_children_from_pos get_int_children_with_pos

  let get_text_children = get_children_from_pos get_text_children_with_pos

  let get_node_children_no_pos node =
    get_children_from_pos (get_node_children node)

  let get_children_no_pos = get_children_from_pos get_children

  let get_child_no_pos_from_child f block =
    f block >>= fun child_with_pos ->
    return (Position.contents child_with_pos)

  let get_int = get_child_no_pos_from_child get_int_with_pos

  let get_text = get_child_no_pos_from_child get_text_with_pos

  let get_node_no_pos node = get_child_no_pos_from_child (get_node node)

  let get_root_node = get_child_no_pos_from_child get_root_node_with_pos


  let extract_from_get get block = extract (get block)

  let extract_int = extract_from_get get_int

  let extract_int_with_pos = extract_from_get get_int_with_pos

  let extract_text = extract_from_get get_text

  let extract_text_with_pos = extract_from_get get_text_with_pos

  let extract_node node = extract_from_get (get_node node)

  let extract_node_no_pos node = extract_from_get (get_node_no_pos node)

  let extract_int_children = extract_from_get get_int_children

  let extract_int_children_with_pos = extract_from_get get_int_children_with_pos

  let extract_text_children = extract_from_get get_text_children

  let extract_text_children_with_pos =
    extract_from_get get_text_children_with_pos

  let extract_node_children node = extract_from_get (get_node_children node)

  let extract_node_children_no_pos node =
    extract_from_get (get_node_children_no_pos node)

  let extract_root_node = extract_from_get get_root_node

  let extract_root_node_with_pos = extract_from_get get_root_node_with_pos

  let extract_children = extract_from_get get_children

  let extract_children_no_pos = extract_from_get get_children_no_pos


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
      let f_pos file line char _ =
	Printf.sprintf "%s@(%d, %d)" file line char in
      let f_no_pos _ = "" in
      Position.apply f_pos f_no_pos block

  let string_of_position block = string_of_position ~debug:false block

  let string_of_primitive = function
    | Int i -> string_of_int i
    | Text s -> string_of_text s

  let rec to_string space block = match Position.contents block with
    | Primitive (Int i) ->
      space ^ (string_of_int i) ^ (string_of_position block)
    | Primitive (Text s) ->
      space ^ (string_of_text s) ^ (string_of_position block)
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
      | Primitive prim ->
	space ^ (Node.to_string name) ^ (string_of_position block) ^
	  " { " ^ (string_of_primitive prim) ^ " }"
      | Node _ ->
	let child = to_string (space ^ "  ") child in
	let name = Node.to_string name in
	space ^ name ^ (string_of_position block) ^
	  " {\n" ^ child ^ "\n" ^ space ^ "}"

  and string_of_children space children =
    List_ext.to_string "\n" (to_string space) children

  let to_string = to_string ""

end
