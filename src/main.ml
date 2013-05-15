
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

let read_file file =
  let ic = open_in file in
  let rec aux s =
    try
      let s' = input_line ic in
      aux (s ^ s' ^ "\n")
    with End_of_file -> s in
  aux ""

let _ = Printf.printf "%s%!" (Html.string (read_file Sys.argv.(1)))
