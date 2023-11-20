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

let pp_propvar out p = CCFormat.string out (pname p)

let print_prop_formula = print_qformula print_propvar
(* If confused by below fn, consider that the input to subfn
   could by a fn f which already "knows" the formula, e.g.
   f: formula -> valuation -> bool, and we pass in
   f fm partial application
*)

let pp_prop_formula out (fm : prop formula) = pp_qformula pp_propvar out fm

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

let negative = function (Not _) -> true | _ -> false
let positive lit = not(negative lit)
let negate = function (Not p) -> p | p -> Not p

let rec nnf = function
| And(p,q) -> And(nnf p,nnf q)
| Or(p,q) -> Or(nnf p,nnf q)
| Imp(p,q) -> Or(nnf(Not p),nnf q)
| Iff(p,q) -> Or(And(nnf p,nnf q),And(nnf(Not p),nnf(Not q)))
| Not(Not p) -> nnf p
| Not(And(p,q)) -> Or(nnf(Not p),nnf(Not q))
| Not(Or(p,q)) -> And(nnf(Not p),nnf(Not q))
| Not(Imp(p,q)) -> And(nnf p,nnf(Not q))
| Not(Iff(p,q)) -> Or(And(nnf p,nnf(Not q)),And(nnf(Not p),nnf q))
| fm -> fm

let nnf fm = nnf(psimplify fm)

let rec nenf  = function
Not(Not p) -> nenf p
| Not(And(p,q)) -> Or(nenf(Not p),nenf(Not q))
| Not(Or(p,q)) -> And(nenf(Not p),nenf(Not q))
| Not(Imp(p,q)) -> And(nenf p,nenf(Not q))
| Not(Iff(p,q)) -> Iff(nenf p,nenf(Not q))
| And(p,q) -> And(nenf p,nenf q)
| Or(p,q) -> Or(nenf p,nenf q)
| Imp(p,q) -> Or(nenf(Not p),nenf q)
| Iff(p,q) -> Iff(nenf p,nenf q)
| fm -> fm

let nenf fm = nenf(psimplify fm)

let list_conj l = if l = [] then True else CCList.reduce_exn (fun x y -> And(x,y)) l

let list_disj l = if l = [] then False else CCList.reduce_exn (fun x y -> Or(x,y)) l

let mk_lits pvs v =
  list_conj (CCList.map (fun p -> if eval p v then p else Not p) pvs)


let rec allsatvaluations subfn v pvs =
match pvs with
  | [] -> if subfn v then [v] else []
  | p::ps -> let v' t q = if q = p then t else v(q) in
    allsatvaluations subfn (v' false) ps @
    allsatvaluations subfn (v' true) ps

let _dnf fm =
  let open CCList in 
  let pvs = atoms fm in
  let satvals = allsatvaluations (eval fm) (fun _ -> false) pvs in
  list_disj (map (mk_lits (map (fun p -> Atom p) pvs)) satvals)

let rec distrib fm =
match fm with
  | And(p,(Or(q,r))) -> Or(distrib(And(p,q)),distrib(And(p,r)))
  | And(Or(p,q),r) -> Or(distrib(And(p,r)),distrib(And(q,r)))
  | _ -> fm

let rec rawdnf fm =
match fm with
  | And(p,q) -> distrib(And(rawdnf p,rawdnf q))
  | Or(p,q) -> Or(rawdnf p,rawdnf q)
  | _ -> fm

let allpairs f l1 l2 =
  let open CCList in
  let* i1 = l1 in
  let+ i2 = l2 in
  f i1 i2


let distrib s1 s2 =
  let cmp = compare in 
  CCList.(sort_uniq (allpairs (union ~eq:(=))  s1 s2) ~cmp)

let rec purednf fm =
  match fm with
  | And(p,q) -> distrib (purednf p) (purednf q)
  | Or(p,q) -> CCList.union ~eq:(=) (purednf p) (purednf q)
  | _ -> [[fm]]

let trivial lits =
  let open CCList in 
  let pos,neg = partition positive lits in
  let im = Util.image negate neg in 
  CCList.inter ~eq:(=) pos (im) <> []

let simpdnf fm =
  if fm = False then [] else if fm = True then [[]] else
    let djs = CCList.filter (CCFun.negate trivial) (purednf(nnf fm)) in
    (* subset doesn't check for proper but original code checks for proper subset. Does this matter?*)
    CCList.filter (fun d -> not(CCList.exists (fun d' -> (CCList.subset ~eq:(=) d' d) && d' <> d) djs)) djs

let dnf fm = list_disj(CCList.map list_conj (simpdnf fm))

let purecnf fm = Util.image (Util.image negate) (purednf(nnf(Not fm)))

let simpcnf fm =
  if fm = False then [[]] else if fm = True then [] else
    let cjs = CCList.filter (CCFun.negate trivial) (purecnf fm) in
    CCList.filter (fun c -> not(CCList.exists (fun c' -> Util.psubset c' c) cjs)) cjs

let cnf fm = list_conj(CCList.map list_disj (simpcnf fm))
