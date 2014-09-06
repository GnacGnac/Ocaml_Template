
module type M = sig
  module T : sig
    type t
    val default : t
    val app_name : string
  end
  module Block : sig
    type t
    module Node : sig type t end
    val parse : string -> (t, Node.t BlockML.parse_error) Result.t
    val save : string -> t -> (unit, BlockML.save_error) Result.t
    val from_block : t -> T.t
    val to_block : T.t -> t
    val analyze : t -> (unit, Node.t BlockML.analyze_error) Result.t
  end
end

module type S = sig
  type t
  type node
  type read_error =
  [`Configuration_read_error of string * node BlockML.parse_error]
  type write_error =
  [`Configuration_write_error of
      string * [BlockML.save_error | node BlockML.analyze_error]]
  val from_file : string -> (t, read_error) Result.t
  val to_file : string -> t -> (unit, write_error) Result.t
  val save : t -> (unit, write_error) Result.t
  val load : unit -> (t, [read_error | write_error]) Result.t
end

module Make (M : M) : S with type t = M.T.t and type node = M.Block.Node.t
