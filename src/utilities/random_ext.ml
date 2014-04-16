
open Result


let () = Random.self_init ()

let keep_numbers s =
  let length = String.length s in
  let rec aux res i =
    if i >= length then res
    else
      let c = s.[i] in
      let res = if '0' <= c && c <= '9' then res ^ (String.make 1 c) else res in
      aux res (i + 1) in
  aux "" 0

let int min max =
  let file = Filename.temp_file "random" "" in
  let cmd = "od -vAn -N4 -tu4 < /dev/urandom > " ^ (Filename.quote file) in
  let res = Sys.command cmd in
  let random () = Big_int.big_int_of_int (Random.int (max - min)) in
  let i =
    if res = 0 then
      (match Sys_ext.read_file file with
      | Ok s -> Big_int.big_int_of_string (keep_numbers s)
      | Error (`Could_not_read_file _) -> random ())
    else random () in
  Sys.remove file ;
  let min = Big_int.big_int_of_int min in
  let max = Big_int.big_int_of_int max in
  let modulo = Big_int.sub_big_int max min in
  let i = Big_int.mod_big_int i modulo in
  Big_int.int_of_big_int (Big_int.add_big_int i min)
