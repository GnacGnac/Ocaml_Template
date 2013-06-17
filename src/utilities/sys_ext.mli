
val save :
  string -> string -> (unit, [> `Could_not_save_in_file of string]) Result.t

val get_env :
  string -> (string, [> `No_such_environment_variable of string]) Result.t

val is_file_empty :
  string ->
  (bool,
   [> `File_does_not_exist of string
    | `Could_not_open_file of string]) Result.t

val remove :
  string ->
  (unit,
   [> `Could_not_remove_file of (string * string)]) Result.t

val read_file : string -> (string, [> `Could_not_open_file of string]) Result.t

val write_file :
  string -> string -> (unit, [> `Could_not_open_file of string]) Result.t
