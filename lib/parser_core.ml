open Base

let matches s = let chars = Base__String.to_list s in
  fun c -> List.mem chars c ~equal:(equal_char)

let space = matches " \t\n\r"
and punctuation = matches "()[]{},"
and symbolic = matches "~`!@#$%^&*-+=|\\:;<>.?/"
and numeric = matches "0123456789"
and alphanumeric = matches
  "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let rec lexwhile prop inp =
  match inp with
    c::cs when prop c -> let tok,rest = lexwhile prop cs in c::tok,rest
  | _ -> [],inp


let rec lex_internal inp =
  match snd(lexwhile space inp) with
  | [] -> []
  | c::cs -> let prop = if alphanumeric(c) then alphanumeric
                        else if symbolic(c) then symbolic
                        else fun _ -> false in
    let toktl,rest = lexwhile prop cs in
    (c::toktl)::lex_internal rest

let lex inp = let res = lex_internal (Base__String.to_list inp) in List.map res ~f:Base.String.of_char_list

let make_parser pfn s =
  let expr,rest = pfn (lex s) in
  if List.is_empty rest then expr else failwith "unparsed input";;
