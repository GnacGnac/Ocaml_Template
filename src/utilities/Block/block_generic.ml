
open Result


module type NODE = sig
  type t
  val to_string : t -> (string, [> `Node_not_bound_to_a_string of t]) Result.t
end


module type S = sig
  module Node : NODE
  type t
  type child = Block of t | Int of int | Text of string
  type children = child list
  val node : Node.t -> children -> t
  val block : t -> child
  val int : int -> child
  val text : string -> child
  val to_string :
    t -> (string, [> `Node_not_bound_to_a_string of Node.t]) Result.t
end


module Make (N : NODE) = struct

  module Node = N


  type t = { name : Node.t ; children : children }

  and children = child list

  and child = Block of t | Int of int | Text of string

  let name block = block.name
  let children block = block.children

  let node name children = { name ; children }

  let block b = Block b
  let int i = Int i
  let text s = Text s


  let string_of_text s = "\"" ^ (String.escaped s) ^ "\""

  let rec to_string space block =
    let children = match children block with
      | [] -> return ""
      | [Int i] -> return ("{ " ^ (string_of_int i) ^ " }")
      | [Text s] -> return ("{ " ^ (string_of_text s) ^ " }")
      | children ->
	string_of_children (space ^ "  ") children >>= fun children ->
	return ("{\n" ^ children ^ "\n" ^ space ^ "}") in
    children >>= fun children ->
    Node.to_string (name block) >>= fun name ->
    return (space ^ name ^ " " ^ children)

  and string_of_children space children =
    List_ext.to_string_err "\n" (string_of_child space) children

  and string_of_child space = function
    | Int i -> return (space ^ (string_of_int i))
    | Text s -> return (space ^ (string_of_text s))
    | Block block -> to_string space block

  let to_string = to_string ""

end
