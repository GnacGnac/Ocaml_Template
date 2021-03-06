
val from_file :
  string ->
  (Xml_parsing.t,
   [> `File_does_not_exist of string
    | `Could_not_open_file of string
    | `Unrecognized_char of char Position.t
    | `Unterminated_comment of unit Position.t
    | `Parse_error of unit Position.t]) Result.t
