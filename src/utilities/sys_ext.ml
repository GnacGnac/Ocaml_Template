
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

let command cmd =
  let res = Sys.command cmd in
  if res = 0 then return ()
  else error (`Command_error (cmd, res))

let open_in file =
  try return (open_in file)
  with Sys_error _ -> error (`Could_not_open_file file)

let is_file_empty file =
  if Sys.file_exists file then
    (open_in file >>= fun ic ->
     try ignore (input_line ic) ; return false
     with End_of_file -> return true)
  else error (`File_does_not_exist file)

let remove file =
  try return (Sys.remove file)
  with Sys_error _ -> error (`Could_not_remove_file file)
