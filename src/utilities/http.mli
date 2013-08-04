
module type S = sig
  module Name : String_ext.OF_STRING
  module Action : String_ext.OF_STRING
  module Html : String_ext.TO_STRING
  val application_name : string
  val inet_addr : string
  val port : int
  val pages :
    Unix.sockaddr ->
    (Action.t, [`Unrecognized_page of string]) Result.t ->
    (Name.t * string) list -> Html.t
end

module Make (S : S) : sig
  val launch : unit -> unit
end

module type UNSAFE_S = sig
  module Name : String_ext.UNSAFE_STRINGABLE
  module Action : String_ext.UNSAFE_STRINGABLE
  module Html : String_ext.TO_STRING
  val application_name : string
  val inet_addr : string
  val port : int
  val pages :
    Unix.sockaddr ->
    (Action.t, [`Unrecognized_page of string]) Result.t ->
    (Name.t * string) list -> Html.t
end

module MakeUnsafe (S : UNSAFE_S) : sig
  val launch : unit -> unit
end
