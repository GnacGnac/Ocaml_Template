
open Result


let save file s =
  try
    let oc = open_out file in
    output_string oc s ;
    close_out oc ;
    return ()
  with Sys_error _ -> error (`Could_not_save_in_file file)


let get_env var =
  try return (Sys.getenv var)
  with Not_found -> error (`No_such_environment_variable var)
