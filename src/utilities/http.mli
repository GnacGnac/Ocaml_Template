
module type S = sig
  module Name : String_ext.OF_STRING
  module Value : String_ext.OF_STRING
  module Action : String_ext.OF_STRING
  module Html : String_ext.TO_STRING
  val port : int
  val pages :
    (Action.t, [> `Unrecognized_page of string]) Result.t ->
    (Name.t * Value.t) list -> Html.t
end

module Make (S : S) : sig
  val launch : unit -> unit
end
