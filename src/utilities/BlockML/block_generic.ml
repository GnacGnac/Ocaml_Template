
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
  val get_ints : t list -> int Position.t list
  val get_texts : t list -> string Position.t list
  val get_nodes : Node.t -> t list -> t list Position.t list
  val get_int_children : t -> int Position.t list
  val get_text_children : t -> string Position.t list
  val get_node_children : Node.t -> t -> t list Position.t list
  val get_int1 : t -> (int Position.t, [> `No_int]) Result.t
  val get_text1 : t -> (string Position.t, [> `No_text]) Result.t
  val get_node1 :
      Node.t -> t -> (t list Position.t, [> `No_such_child]) Result.t
  val get_children : t -> (t list, [> `No_children]) Result.t
  val to_string : t -> string

  (* Unsafe functions: raises assertion failure. *)
  val extract_get : (t list -> ('a Position.t, 'b) Result.t) -> t list -> 'a
  val extract_int : t list -> int
  val extract_text : t list -> string
  val extract_node : Node.t -> t list -> t list
  val extract_int1 : t -> int
  val extract_text1 : t -> string
  val extract_child_node : Node.t -> t -> t list
  val extract_children : t -> t list
end


module Make (N : NODE) = struct

  module Node = N


  type primitive = Int of int | Text of string
  type contents = Primitive of primitive | Node of Node.t * t list
  and t = contents Position.t

  let int_content i = Primitive (Int i)
  let text_content s = Primitive (Text s)
  let node_content name children = Node (name, children)

  let int i = Position.make_dummy (int_content i)
  let text s = Position.make_dummy (text_content s)
  let node name children = Position.make_dummy (node_content name children)

  let get_children block = match Position.contents block with
    | Node (_, children) -> return children
    | _ -> error `No_children

  let gets f l =
    let f' res a =
      (match f (Position.contents a) with
	| None -> []
	| Some e -> [Position.change_contents e a]) @ res in
    List.fold_left f' [] l

  let get_ints = gets (function Primitive (Int i) -> Some i | _ -> None)
  let get_texts = gets (function Primitive (Text s) -> Some s | _ -> None)
  let get_nodes node =
    gets
      (function
	| Node (node', children) when node' = node -> Some children 
	| _ -> None)

  let get_from_children f block = match Position.contents block with
    | Node (_, children) -> f children
    | _ -> []

  let get_int_children = get_from_children get_ints
  let get_text_children = get_from_children get_texts
  let get_node_children node = get_from_children (get_nodes node)

  let get f res_error l = match f l with
    | [] -> error res_error
    | a :: _ -> return a

  let get_int = get get_ints `No_int
  let get_text = get get_texts `No_text
  let get_node node = get (get_nodes node) `No_such_child

  let get1_from_children f res_error block = match f block with
    | [] -> error res_error
    | a :: _ -> return a

  let get_int1 = get1_from_children get_ints `No_int
  let get_text1 = get1_from_children get_texts `No_text
  let get_node1 node = get1_from_children (get_nodes node) `No_such_node

  let extract_get get e = Position.contents (extract (get e))
  let extract_int = extract_get get_int
  let extract_text = extract_get get_text
  let extract_node node = extract_get (get_node node)
  let extract_children node = assert false (* extract_get get_children [node] *)
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
