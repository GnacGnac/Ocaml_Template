
val options : (Arg.key * Arg.spec * Arg.doc) list ref
val register : (Arg.key * Arg.spec * Arg.doc) list -> unit
val set_args : string -> unit
val usage_msg : unit -> string
val results : unit -> string list
