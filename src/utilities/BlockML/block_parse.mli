
val from_file :
  string ->
  (Block_string.t,
   [> `File_does_not_exist of string
    | `Could_not_open_file of string
    | `Unrecognized_char of char * Position.t
    | `Parse_error of Position.t]) Result.t
