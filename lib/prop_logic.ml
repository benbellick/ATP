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

let negate p = Not (p)

let pname(P s) = s

let parse_propvar _vs inp =
  match inp with
  | p::oinp when p <> "(" -> Atom(P(p)),oinp
  | _ -> failwith "parse_propvar"

let parse_prop_formula = make_parser
    (parse_formula ((fun _ _ -> failwith ""),parse_propvar) [])

let default_parser = parse_prop_formula

let print_propvar _prec p = Format.print_string(pname p)

let print_prop_formula = print_qformula print_propvar
(* If confused by below fn, consider that the input to subfn
   could by a fn f which already "knows" the formula, e.g.
   f: formula -> valuation -> bool, and we pass in
   f fm partial application
*)
let rec onallvaluations subfn v ats =
  match ats with
  | [] -> subfn v
  | p::ps -> let v' t q = if q = p then t else v(q) in
    onallvaluations subfn (v' false) ps &&
    onallvaluations subfn (v' true) ps

let rec to_string = function
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
  print_string(CCList.fold_right (fun s t -> fixw(pname s) ^ t) ats "| formula");
  print_newline(); print_string separator; print_newline();
  let _ = onallvaluations mk_row (fun _x -> false) ats in
  print_string separator; print_newline()


let tautology fm = 
  onallvaluations (eval fm) (fun _ -> false) (atoms fm)

let unsatisfiable fm = tautology( negate fm)
let satisfiable fm = not (unsatisfiable fm)

let psubst subfn = onatoms (fun p -> Fpf.tryapplyd subfn p (Atom p))

let rec dual = function
  | False -> True
  | True -> False
  | Atom(p) -> Atom(p)
  | Not(p) -> Not(dual p)
  | And(p,q) -> Or(dual p, dual q)
  | Or(p,q) -> And(dual p, dual q)
  | _ -> failwith "Formula contains ==> or <=>"
    

let psimplify1 = function 
  | Not False -> True
  | Not True -> False
  | Not(Not p) -> p
  | And(_,False) | And(False,_) -> False
  | And(p,True) | And(True,p) -> p
  | Or(p,False) | Or(False,p) -> p
  | Or(_,True) | Or(True,_) -> True
  | Imp(False,_) | Imp(_,True) -> True
  | Imp(True,p) -> p
  | Imp(p,False) -> Not p
  | Iff(p,True) | Iff(True,p) -> p
  | Iff(p,False) | Iff(False,p) -> Not p
  | fm -> fm

let rec psimplify = function
  | Not p -> psimplify1 (Not(psimplify p))
  | And(p,q) -> psimplify1 (And(psimplify p,psimplify q))
  | Or(p,q) -> psimplify1 (Or(psimplify p,psimplify q))
  | Imp(p,q) -> psimplify1 (Imp(psimplify p,psimplify q))
  | Iff(p,q) -> psimplify1 (Iff(psimplify p,psimplify q))
  | fm -> fm
