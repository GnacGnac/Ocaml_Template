
open Amount
open Bijection
open Error
open Html
open Http
open List_ext
open Map_ext
open Option
open Position
open Result
open Set_ext
open String_ext
open Sys_ext
open BlockML
open Debug
open Grammar_example

let editable_infos =
  Html.EditableInfos.make
    [Html.input () ; Html.input ()] "Add" "add" "Performzhzrhzruyzyhyh" "perform"
    [("Deleteagzeyzryry", "delete") ; ("Edit", "edit")]

let table =
  Html.html
    [Html.body
	[Html.result_table ~border:1 ~cellspacing:0 ~cellpadding:4
	    "/" "My table" string_of_int "GET" ["Name" ; "Value"]
	    ~editable_infos
	    (List.map (List.map Html.text_string)
	       [["Lau" ; "36"] ; ["Nico & Lau" ; "Céur"]])]]
let _ = Printf.printf "%s\n%!" (Html.to_string table)
