
let options = ref []

let register o = 
  options := o @ !options

let args = ref ""
let set_args = (:=) args

let usage_msg () =
  let exe = Filename.basename Sys.executable_name in
  "Usage: " 
  ^ exe
  ^ " [options] " ^ !args ^ "\n"
  ^ "Type: `" ^ exe ^ " -help' to list options."

let results () = 
  let extra_arguments = ref [] in
  Arg.parse (Arg.align !options)
    (fun s -> extra_arguments := s :: !extra_arguments) 
    (usage_msg ()) ;
  !extra_arguments
