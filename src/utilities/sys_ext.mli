
val save :
  string -> string -> (unit, [> `Could_not_save_in_file of string]) Result.t

val get_env :
  string -> (string, [> `No_such_environment_variable of string]) Result.t

val command : string -> (unit, [> `Command_error of (string * int)]) Result.t
