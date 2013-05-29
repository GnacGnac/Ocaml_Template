
let show_with_prefix prefix msg = Printf.eprintf "** %s:\n   %s\n%!" prefix msg

let show = show_with_prefix "Error"
let show_warning = show_with_prefix "Warning"
