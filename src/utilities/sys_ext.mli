
val get_env :
  string -> (string, [> `No_such_environment_variable of string]) Result.t

val is_file_empty :
  string ->
  (bool,
   [> `File_does_not_exist of string
    | `Could_not_open_in_file of string]) Result.t

val remove :
  ?f:bool ->
  string ->
  (unit, [> `Could_not_remove_file of (string * int)]) Result.t

val read_file : string -> (string, [> `Could_not_read_file of string]) Result.t

val write_file :
  string -> string -> (unit, [> `Could_not_write_file of string]) Result.t

val chdir :
  string -> (unit, [> `Could_not_change_to_directory of string]) Result.t

val mkdir :
  ?p:bool -> string ->
  (unit, [> `Could_not_create_directory of string]) Result.t

val readdir : string -> (string array, [> `Not_a_directory of string]) Result.t
