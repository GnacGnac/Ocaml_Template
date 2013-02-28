
open Result


type param_name = string
type param_value = string
type params = (string * string) list

let make_url url params =
  let f_param (name, value) = name ^ "=" ^ value in
  let params = "?" ^ (List_ext.to_string "&" f_param params) in
  url ^ params

let get_param_value params name =
  try return (List.assoc name params)
  with Not_found -> error (`No_such_parameter name)

let mem_params params name = List.mem_assoc name params

let make_params l = l


module type S = sig
  val port : int
  val pages : string -> params -> Html.t
end


module Make (S : S) = struct

  let url_special_chars =
    [("+", " ") ;
     ("%C3%80", "À") ; ("%C3%82", "Â") ; ("%C3%84", "Ä") ;
     ("%C3%A0", "à") ; ("%C3%A2", "â") ; ("%C3%A4", "ä") ;
     ("%C3%89", "É") ; ("%C3%88", "È") ; ("%C3%8A", "Ê") ; ("%C3%8B", "Ë") ;
     ("%C3%A9", "é") ; ("%C3%A8", "è") ; ("%C3%AA", "ê") ; ("%C3%AB", "ë") ;
     ("%C3%8E", "Î") ; ("%C3%8F", "Ï") ; ("%C3%AE", "î") ; ("%C3%AF", "ï") ;
     ("%C3%94", "Ô") ; ("%C3%B4", "ô") ;
     ("%C3%99", "Ù") ; ("%C3%9C", "Ü") ; ("%C3%B9", "ù") ; ("%C3%BC", "ü") ;
     ("%C5%92", "Œ") ; ("%C5%93", "œ") ;
     ("%C0", "À") ; ("%C2", "Â") ; ("%C4", "Ä") ;
     ("%E0", "à") ; ("%E2", "â") ; ("%E4", "ä") ;
     ("%C9", "É") ; ("%C8", "È") ; ("%CA", "Ê") ; ("%CB", "Ë") ;
     ("%E9", "é") ; ("%E8", "è") ; ("%EA", "ê") ; ("%EB", "ë") ;
     ("%CE", "Î") ; ("%CF", "Ï") ; ("%EE", "î") ; ("%EF", "ï") ;
     ("%D4", "Ô") ; ("%F4", "ô") ;
     ("%D9", "Ù") ; ("%DC", "Ü") ; ("%F9", "ù") ; ("%FC", "ü") ;
     ("%8C", "Œ") ; ("%9C", "œ") ;
     ("%27", "'") ;
     ("%24", "$") ; ("%26", "&") ; ("%2B", "+") ; ("%2C", ",") ; ("%2F", "/") ;
     ("%3A", ":") ; ("%3B", ";") ; ("%3D", "=") ; ("%3F", "?") ; ("%40", "@") ;
     ("%20", " ") ; ("%22", "\"") ; ("%28", "(") ; ("%29", ")") ;
     ("%3C", "<") ; ("%3E", ">") ; ("%23", "#") ;
     ("%7B", "{") ; ("%7D", "}") ; ("%7C", "|") ; ("%5C", "\\") ; ("%5E", "^") ;
     ("%7E", "~") ; ("%5B", "[") ; ("%5D", "]") ; ("%60", "`") ; ("%25", "%")]

  let replace_url_special_char (char, replacement) s =
    Str.global_replace (Str.regexp char) replacement s

  let replace_url_special_chars s =
    let rec aux s = function
      | [] -> s
      | url_special_char :: url_special_chars ->
	aux (replace_url_special_char url_special_char s) url_special_chars in
    aux s url_special_chars

  let string_of_param_value value = replace_url_special_chars value

  let param_name_value p =
    let index = String.index_from p 0 '=' in
    let name = String.sub p 0 index in
    let value = String.sub p (index+1) ((String.length p) - (index+1)) in
    let value = string_of_param_value value in
    (name, value)

  let get_params url =
    if String.contains url '?' then
      let index = (String.index_from url 0 '?') + 1 in
      let length = (String.length url) - index in
      let params = String.sub url index length in
      let params = Str.split (Str.regexp "&") params in
      List.map param_name_value params
    else []

  let words s = Str.split (Str.regexp "[ \t]+") s

  let requested_page url =
    if String.contains url '?' then
      let index = String.index_from url 0 '?' in
      String.sub url 0 index
    else url

  let treat_url outc url =
    let page = requested_page url in
    let params = get_params url in
    let output = Html.to_string (S.pages page params) in
    output_string outc output

  let is_endline s = (String.length s = 1) && (Char.code s.[0] = 13)

  type kind =
    | No_kind
    | Get of string
    | Post of string * int
    | Content_length of int

  let check_kind s = match words s with
    | get :: url :: protocol :: _
	when (String.lowercase get = "get") &&
   	     (String.length protocol >= 7) &&
  	     (String.lowercase (String.sub protocol 0 7) = "http/1.") ->
      Get url
    | post :: url :: protocol :: _
	when (String.lowercase post = "post") &&
   	     (String.length protocol >= 7) &&
  	     (String.lowercase (String.sub protocol 0 7) = "http/1.") ->
      Post (url, 0)
    | content_length :: length :: _
	when String.lowercase content_length = "content-length:" ->
      let length =
	try int_of_string (String.sub length 0 (String.length length - 1))
	with Failure _ -> 0 in
      Content_length length
    | s :: _ -> No_kind
    | _ -> No_kind

  let treat_message inc outc =
    let rec aux kind =
      try
	let s = input_line inc in
	let is_endline = is_endline s in
	match kind, check_kind s with
	  | (Get url, _) when is_endline -> treat_url outc url
	  | (Post (url, length), _) when is_endline ->
	    let params = String.make length '*' in
	    let _ = input inc params 0 length in
	    treat_url outc (url ^ "?" ^ params)
	  | (Post (url, _), Content_length length) -> aux (Post (url, length))
	  | (_, ((Get _ | Post _) as kind)) -> aux kind
	  | _ -> aux kind
      with End_of_file -> () in
    aux No_kind

  let localhost = Unix.inet_addr_of_string "127.0.0.1"

  let launch () =
    let sockaddr = Unix.ADDR_INET (localhost, S.port) in
    Unix.establish_server treat_message sockaddr

end
