
type t = { name : string ; children : children }

and children = child list

and child = Text of string | Block of t

let name block = block.name
let children block = block.children

let node name children = { name ; children }

let block b = Block b

let text s = Text s


let string_of_text s = "\"" ^ (String.escaped s) ^ "\""

let rec to_string space block =
  let children = match children block with
    | [] -> ""
    | [Text s] -> "{ " ^ (string_of_text s) ^ " }"
    | children ->
      "{\n" ^ (string_of_children (space ^ "  ") children) ^ "\n" ^
	space ^ "}" in
  space ^ (name block) ^ " " ^ children

and string_of_children space children =
  List_ext.to_string "\n" (string_of_child space) children

and string_of_child space = function
  | Text s -> space ^ (string_of_text s)
  | Block block -> to_string space block

let to_string = to_string ""
