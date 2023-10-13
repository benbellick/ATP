open Formula_core
open Parser_core
open CCFun

type prop = P of string

let rec eval fm v =
  match fm with
  | False -> false
  | True -> true
  | Atom(x) -> v x
  | Not(p) -> not (eval p v)
  | And(p,q) -> eval p v && eval q v
  | Or(p,q) -> eval p v || eval q v
  | Imp(p,q) -> not(eval p v) || eval q v
  | Iff(p,q) -> (eval p v) = (eval q v)
  | Forall(_,_) -> failwith "Forall not prop"
  | Exists(_,_) -> failwith "Exists no prop"


let pname(P s) = s

let parse_propvar _vs inp =
  match inp with
  | p::oinp when p <> "(" -> Atom(P(p)),oinp
  | _ -> failwith "parse_propvar"

let parse_prop_formula = make_parser
    (parse_formula ((fun _ _ -> failwith ""),parse_propvar) [])

let default_parser = parse_prop_formula

let print_propvar _prec p = print_string(pname p)

let print_prop_formula = print_qformula print_propvar

let rec onallvaluations subfn v ats =
  match ats with
  | [] -> subfn v
  | p::ps -> let v' t q = if q = p then t else v(q) in
    onallvaluations subfn (v' false) ps &&
    onallvaluations subfn (v' true) ps

let rec to_string fm = match fm with
  | Atom a -> pname a
  | Not(p) -> "~" ^ to_string p
  | And(p,q) -> to_string p ^ " /\\ " ^ to_string q
  | Or(p,q) -> to_string p ^ " \\/ " ^ to_string q
  | Imp(p,q) -> to_string p ^ " ==> " ^ to_string q
  | Iff(p,q) ->  to_string p ^ " <==> " ^ to_string q
  | _ -> failwith "not a formula"



let print_truthtable fm =
  let ats = atoms fm in
  let width = CCList.fold_right (max % String.length % pname) ats 5 + 1 in
  let fixw s = s^String.make(width - String.length s) ' ' in
  let truthstring p = fixw (if p then "true" else "false") in
  let mk_row v =
     let lis = CCList.map (fun x -> truthstring(v x)) ats
     and ans = truthstring(eval fm v) in
     print_string(CCList.fold_right (^) lis ("| "^ans)); print_newline(); true in
  let separator = String.make (width * List.length ats + 9) '-' in
  print_string(CCList.fold_right (fun s t -> fixw(pname s) ^ t) ats "| " ^ to_string fm);
  print_newline(); print_string separator; print_newline();
  let _ = onallvaluations mk_row (fun _x -> false) ats in
  print_string separator; print_newline()


