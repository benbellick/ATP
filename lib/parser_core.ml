let rec do_list f l =
  match l with
    [] -> ()
  | h::t -> f(h); do_list f t

let space = CCString.contains " \t\n\r"
and punctuation = CCString.contains "()[]{},"
and symbolic =CCString.contains  "~`!@#$%^&*-+=|\\:;<>.?/"
and numeric = CCString.contains "0123456789"
and alphanumeric = CCString.contains "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

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

let lex inp =
  let res = lex_internal (CCString.to_list inp) in
  List.map CCString.of_list res

let make_parser pfn s =
  let expr,rest = pfn (lex s) in
  if CCList.is_empty rest then expr else failwith "unparsed input"


(* This is lifted directly from the appendix without understanding the code *)
let papply f (ast,rest) = (f ast,rest)

let nextin inp tok = inp <> [] && List.hd inp = tok

let parse_bracketed subparser cbra inp =
  let ast,rest = subparser inp in
  if nextin rest cbra then ast, List.tl rest
  else failwith "Closing bracket expected"

let rec parse_ginfix opsym opupdate sof subparser inp =
  let e1,inp1 = subparser inp in
  if inp1 <> [] && List.hd inp1 = opsym then
     parse_ginfix opsym opupdate (opupdate sof e1) subparser (List.tl inp1)
  else sof e1,inp1

let parse_left_infix opsym opcon =
  parse_ginfix opsym (fun f e1 e2 -> opcon(f e1,e2)) (fun x -> x)

let parse_right_infix opsym opcon =
  parse_ginfix opsym (fun f e1 e2 -> f(opcon(e1,e2))) (fun x -> x)

let parse_list opsym =
  parse_ginfix opsym (fun f e1 e2 -> (f e1)@[e2]) (fun x -> [x])


let bracket p n f x y =
  let open Format in
  (if p then print_string "(" else ());
  open_box n; f x y; close_box();
  (if p then print_string ")" else ())


