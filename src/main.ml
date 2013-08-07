
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


module S = struct

  module Name = struct type t = unit let string_assoc = [] end
  module Action = struct type t = unit let string_assoc = [] end

end

module Html = Html.MakeUnsafe (S)

let () =
  let node =
    Html.html
      [Html.body ~class_:"navbar"
	  [Html.div ~style:[Html.Color Html.red ; Html.Text_align Html.Center]
	      [Html.text "Coucou !" ;
	       Html.input ~type_:Html.Submit ~value:"Connexion" ()]]] in
  Printf.printf "%s\n%!" (Html.to_string node)
