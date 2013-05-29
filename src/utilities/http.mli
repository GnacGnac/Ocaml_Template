
(*
type param_name = string
type param_value = string
type params

val get_param_value :
  params -> param_name ->
  (param_value, [> `No_such_parameter of string]) Result.t

module type S = sig
  val port : int
  val pages : string -> params -> Html.t
end

module Make (S : S) : sig
  val launch : unit -> unit
end
*)
