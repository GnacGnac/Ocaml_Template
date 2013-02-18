
open Ocamlbuild_plugin;;

dispatch begin function
  | After_rules -> ocaml_lib "src/utilities/blockML"
  | _ -> ()
end
