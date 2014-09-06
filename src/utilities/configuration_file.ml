
open Result


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


module Make (M : M) : S with type t = M.T.t and type node = M.Block.Node.t =
struct

  type t = M.T.t

  type node = M.Block.Node.t

  type read_error =
  [`Configuration_read_error of string * node BlockML.parse_error]

  type write_error =
  [`Configuration_write_error of
      string * [BlockML.save_error | node BlockML.analyze_error]]

  let configuration_file =
    (match Sys_ext.get_env "HOME" with
    | Ok home -> Filename.concat home
    | Error _ -> (fun file -> file))
      ("." ^ M.T.app_name ^ "rc")

  let to_block conf = 
    let block = M.Block.to_block conf in
    M.Block.analyze block >>= fun () ->
    return block

  let from_file file =
    map_error (fun error -> `Configuration_read_error (file, error))
      (M.Block.parse file >>= (M.Block.from_block |> return))

  type 'a to_file =
    ('a, [node BlockML.analyze_error | BlockML.save_error]) Result.t

  let to_file file conf =
    map_error (fun error -> `Configuration_write_error (file, error))
      ((to_block conf     :> M.Block.t to_file) >>=
       (M.Block.save file :> M.Block.t -> unit to_file))

  let save = to_file configuration_file

  let load () =
    if Sys.file_exists configuration_file then from_file configuration_file
    else save M.T.default >>= fun () -> return M.T.default

end
