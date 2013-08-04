
open Result


let get_env var =
  try return (Sys.getenv var)
  with Not_found -> error (`No_such_environment_variable var)

let open_in file =
  try return (open_in file)
  with Sys_error _ -> error (`Could_not_open_in_file file)

let open_out file =
  try return (open_out file)
  with Sys_error _ -> error (`Could_not_open_out_file file)

let is_file_empty file =
  if Sys.file_exists file then
    (open_in file >>= fun ic ->
     try ignore (input_line ic) ; return false
     with End_of_file -> return true)
  else error (`File_does_not_exist file)

let remove file =
  try return (Sys.remove file)
  with Sys_error err -> error (`Could_not_remove_file (file, err))

let read_file file =
  map_error (function `Could_not_open_in_file s -> `Could_not_read_file s)
    (open_in file >>= fun ic ->
     let rec aux s =
       try
	 let s' = input_line ic in
	 aux (s ^ s' ^ "\n")
       with End_of_file -> s in
     let res = aux "" in
     close_in ic ;
     return res)

let write_file file s =
  map_error (function `Could_not_open_out_file s -> `Could_not_write_file s)
    (open_out file >>= fun oc ->
     output_string oc s ;
     close_out oc ;
     return ())
