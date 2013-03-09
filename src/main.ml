
open Result
open Http


module StringBlock = struct

  module M = struct

    module Ord = struct
      type t = string
      let compare = Pervasives.compare
    end

    include Ord

    module Set = Set_ext.Make (Ord)

    let to_string s = s
    let of_string s = return s

    module Children = BlockML.ChildrenSpec.Make (Ord)

    let spec = function
      | _ ->
	Children.make
	  BlockML.Occurrence.any BlockML.Occurrence.any Children.NodeMap.empty

  let possible_roots = Set.singleton "yop"

  end

  include BlockML.Instance.Make (M)

end

(*
let string_of_error = function
  | `Bad_int_occurrence (node, occurrence, possible_occurrences) ->
    Printf.sprintf "bad int children occurrence of node %s (%d not in %s)"
      (BlockString.Node.to_string node)
      occurrence (BlockML.Occurrence.to_string possible_occurrences)
  | `Bad_text_occurrence (node, occurrence, possible_occurrences) ->
    Printf.sprintf "bad text children occurrence (%d not in %s)"
      occurrence (BlockML.Occurrence.to_string possible_occurrences)
  | `Bad_sub_node_occurrence
      (node, sub_node, occurrence, possible_occurrences) ->
    Printf.sprintf "bad text children occurrence (%d not in %s)"
      occurrence (BlockML.Occurrence.to_string possible_occurrences)
  | `File_does_not_exist file -> "file " ^ file ^ " does not exist"
  | `Could_not_open_file file -> "could not open file " ^ file
  | `Unrecognized_char pos ->
    let f_pos line char _ = Printf.sprintf "line %d character %d, " line char in
    let f_no_pos _ = "" in
    let prefix = Position.apply f_pos f_no_pos pos in
    Printf.sprintf "%sunrecognized character '%c'"
      prefix (Position.contents pos)
  | `Parse_error pos ->
    let f_pos line char _ = Printf.sprintf " %d character %d" line char in
    let f_no_pos _ = "" in
    let suffix = Position.apply f_pos f_no_pos pos in
    Printf.sprintf "parse error line%s" suffix
  | `Not_a_root_node None -> "root is not a node"
  | `Not_a_root_node (Some node) -> "node is not a root node"
  | `Unrecognized_node node -> "unrecognized node"
*)

module HttpServerConf = struct

  let port = 28000

  let index params =
    let name = "value" in
    let i = try (match Http.get_param_value params name with
      | Ok i -> int_of_string i
      | Error (`No_such_parameter _) -> 105154) with _ -> 105154 in
    let i = string_of_int (i + 1) in
    Html.html
      [Html.body
	  [Html.form ~method_:"GET" ~action:"/"
	      [Html.text i ; Html.input ~type_:"hidden" ~name ~value:i () ;
	       Html.br ;
	       Html.input ~type_:"submit" ~value:"Next" ()]]]

  let not_found page _ =
    Html.html [Html.body [Html.text (page ^ " not found")]]

  let pages = function
    | "/" -> index
    | s -> not_found s

end

module HttpServer = Http.Make (HttpServerConf)


let _ = (* match StringBlock.parse Sys.argv.(1) with
  | Ok block -> Printf.printf "%s\n%!" (StringBlock.to_string block)
  | _ -> () *)
  HttpServer.launch ()
(*
  let html =
    Html.html
      [Html.body
	  [Html.text "Hello world!" ;
	   Html.input ~typ:"hidden" ()]] in
  Printf.printf "%s\n%!" (Html.to_string html)
*)
