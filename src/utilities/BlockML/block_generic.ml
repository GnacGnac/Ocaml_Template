
open Result


module type NODE = sig
  type t
  val to_string : t -> (string, [> `Node_not_bound_to_a_string of t]) Result.t
end


module type S = sig
  module Node : NODE
  type t = Int of int | Text of string | Node of Node.t * t list
  val int : int -> t
  val text : string -> t
  val node : Node.t -> t list -> t
  val to_string :
    t -> (string, [> `Node_not_bound_to_a_string of Node.t]) Result.t
end


module Make (N : NODE) = struct

  module Node = N


  type t = Int of int | Text of string | Node of Node.t * t list

  let int i = Int i
  let text s = Text s
  let node name children = Node (name, children)


  let string_of_text s = "\"" ^ (String.escaped s) ^ "\""

  let rec to_string space = function
    | Int i -> return (space ^ (string_of_int i))
    | Text s -> return (space ^ (string_of_text s))
    | Node (name, []) ->
      Node.to_string name >>= fun name -> return (space ^ name)
    | Node (name, [Int i]) ->
      Node.to_string name >>= fun name ->
      return (space ^ name ^ " { " ^ (string_of_int i) ^ " }")
    | Node (name, [Text s]) ->
      Node.to_string name >>= fun name ->
      return (space ^ name ^ " { " ^ (string_of_text s) ^ " }")
    | Node (name, children) ->
      string_of_children (space ^ "  ") children >>= fun children ->
      Node.to_string name >>= fun name ->
      return (space ^ name ^ " {\n" ^ children ^ "\n" ^ space ^ "}")

  and string_of_children space children =
    List_ext.to_string_err "\n" (to_string space) children

  let to_string = to_string ""

end
