open Formula_core
open Parser_core
open CCFun

type prop = P of string

let rec eval fm v =
  match fm with
  | False -> false
  | True -> true
  | Atom x -> v x
  | Not p -> not (eval p v)
  | And (p, q) -> eval p v && eval q v
  | Or (p, q) -> eval p v || eval q v
  | Imp (p, q) -> (not (eval p v)) || eval q v
  | Iff (p, q) -> eval p v = eval q v
  | Forall (_, _) -> failwith "Forall not prop"
  | Exists (_, _) -> failwith "Exists no prop"

let negate p = Not p
let pname (P s) = s

let parse_propvar _vs inp =
  match inp with
  | p :: oinp when p <> "(" -> (Atom (P p), oinp)
  | _ -> failwith "parse_propvar"

let parse_prop_formula =
  make_parser (parse_formula ((fun _ _ -> failwith ""), parse_propvar) [])

let default_parser = parse_prop_formula
let print_propvar _prec p = Format.print_string (pname p)
let pp_propvar out p = CCFormat.string out (pname p)
let print_prop_formula = print_qformula print_propvar
let pp_prop_formula = pp_qformula pp_propvar

(* If confused by below fn, consider that the input to subfn
   could by a fn f which already "knows" the formula, e.g.
   f: formula -> valuation -> bool, and we pass in
   f fm partial application
*)

let rec onallvaluations subfn v ats =
  match ats with
  | [] -> subfn v
  | p :: ps ->
      let v' t q = if q = p then t else v q in
      onallvaluations subfn (v' false) ps && onallvaluations subfn (v' true) ps

let rec to_string = function
  | Atom a -> pname a
  | Not p -> "~" ^ to_string p
  | And (p, q) -> to_string p ^ " /\\ " ^ to_string q
  | Or (p, q) -> to_string p ^ " \\/ " ^ to_string q
  | Imp (p, q) -> to_string p ^ " ==> " ^ to_string q
  | Iff (p, q) -> to_string p ^ " <==> " ^ to_string q
  | _ -> failwith "not a formula"

let print_truthtable fm =
  let ats = atoms fm in
  let width = CCList.fold_right (max % String.length % pname) ats 5 + 1 in
  let fixw s = s ^ String.make (width - String.length s) ' ' in
  let truthstring p = fixw (if p then "true" else "false") in
  let mk_row v =
    let lis = CCList.map (fun x -> truthstring (v x)) ats
    and ans = truthstring (eval fm v) in
    print_string (CCList.fold_right ( ^ ) lis ("| " ^ ans));
    print_newline ();
    true
  in
  let separator = String.make ((width * List.length ats) + 9) '-' in
  print_string
    (CCList.fold_right (fun s t -> fixw (pname s) ^ t) ats "| formula");
  print_newline ();
  print_string separator;
  print_newline ();
  let _ = onallvaluations mk_row (fun _x -> false) ats in
  print_string separator;
  print_newline ()

let tautology fm = onallvaluations (eval fm) (fun _ -> false) (atoms fm)
let unsatisfiable fm = tautology (negate fm)
let satisfiable fm = not (unsatisfiable fm)
let psubst subfn = onatoms (fun p -> Fpf.tryapplyd subfn p (Atom p))

let rec dual = function
  | False -> True
  | True -> False
  | Atom p -> Atom p
  | Not p -> Not (dual p)
  | And (p, q) -> Or (dual p, dual q)
  | Or (p, q) -> And (dual p, dual q)
  | _ -> failwith "Formula contains ==> or <=>"

let psimplify1 = function
  | Not False -> True
  | Not True -> False
  | Not (Not p) -> p
  | And (_, False) | And (False, _) -> False
  | And (p, True) | And (True, p) -> p
  | Or (p, False) | Or (False, p) -> p
  | Or (_, True) | Or (True, _) -> True
  | Imp (False, _) | Imp (_, True) -> True
  | Imp (True, p) -> p
  | Imp (p, False) -> Not p
  | Iff (p, True) | Iff (True, p) -> p
  | Iff (p, False) | Iff (False, p) -> Not p
  | fm -> fm

let rec psimplify = function
  | Not p -> psimplify1 (Not (psimplify p))
  | And (p, q) -> psimplify1 (And (psimplify p, psimplify q))
  | Or (p, q) -> psimplify1 (Or (psimplify p, psimplify q))
  | Imp (p, q) -> psimplify1 (Imp (psimplify p, psimplify q))
  | Iff (p, q) -> psimplify1 (Iff (psimplify p, psimplify q))
  | fm -> fm

let negative = function Not _ -> true | _ -> false
let positive lit = not (negative lit)
let negate = function Not p -> p | p -> Not p

let rec nnf = function
  | And (p, q) -> And (nnf p, nnf q)
  | Or (p, q) -> Or (nnf p, nnf q)
  | Imp (p, q) -> Or (nnf (Not p), nnf q)
  | Iff (p, q) -> Or (And (nnf p, nnf q), And (nnf (Not p), nnf (Not q)))
  | Not (Not p) -> nnf p
  | Not (And (p, q)) -> Or (nnf (Not p), nnf (Not q))
  | Not (Or (p, q)) -> And (nnf (Not p), nnf (Not q))
  | Not (Imp (p, q)) -> And (nnf p, nnf (Not q))
  | Not (Iff (p, q)) -> Or (And (nnf p, nnf (Not q)), And (nnf (Not p), nnf q))
  | fm -> fm

let nnf fm = nnf (psimplify fm)

let rec nenf = function
  | Not (Not p) -> nenf p
  | Not (And (p, q)) -> Or (nenf (Not p), nenf (Not q))
  | Not (Or (p, q)) -> And (nenf (Not p), nenf (Not q))
  | Not (Imp (p, q)) -> And (nenf p, nenf (Not q))
  | Not (Iff (p, q)) -> Iff (nenf p, nenf (Not q))
  | And (p, q) -> And (nenf p, nenf q)
  | Or (p, q) -> Or (nenf p, nenf q)
  | Imp (p, q) -> Or (nenf (Not p), nenf q)
  | Iff (p, q) -> Iff (nenf p, nenf q)
  | fm -> fm

let nenf fm = nenf (psimplify fm)

let list_conj l =
  if l = [] then True else CCList.reduce_exn (fun x y -> And (x, y)) l

let list_disj l =
  if l = [] then False else CCList.reduce_exn (fun x y -> Or (x, y)) l

let mk_lits pvs v =
  list_conj (CCList.map (fun p -> if eval p v then p else Not p) pvs)

let rec allsatvaluations subfn v pvs =
  match pvs with
  | [] -> if subfn v then [ v ] else []
  | p :: ps ->
      let v' t q = if q = p then t else v q in
      allsatvaluations subfn (v' false) ps @ allsatvaluations subfn (v' true) ps

let _dnf fm =
  let open CCList in
  let pvs = atoms fm in
  let satvals = allsatvaluations (eval fm) (fun _ -> false) pvs in
  list_disj (map (mk_lits (map (fun p -> Atom p) pvs)) satvals)

let rec distrib fm =
  match fm with
  | And (p, Or (q, r)) -> Or (distrib (And (p, q)), distrib (And (p, r)))
  | And (Or (p, q), r) -> Or (distrib (And (p, r)), distrib (And (q, r)))
  | _ -> fm

let rec rawdnf fm =
  match fm with
  | And (p, q) -> distrib (And (rawdnf p, rawdnf q))
  | Or (p, q) -> Or (rawdnf p, rawdnf q)
  | _ -> fm

let allpairs f l1 l2 =
  let open CCList in
  let* i1 = l1 in
  let+ i2 = l2 in
  f i1 i2

let distrib s1 s2 =
  let cmp = compare in
  CCList.(sort_uniq (allpairs (union ~eq:( = )) s1 s2) ~cmp)

let rec purednf fm =
  match fm with
  | And (p, q) -> distrib (purednf p) (purednf q)
  | Or (p, q) -> CCList.union ~eq:( = ) (purednf p) (purednf q)
  | _ -> [ [ fm ] ]

(*Check checks to see if there are any complimentary literals in the same list*)
(* e.g. trivial {p, q, ~p} = true. What to do with a trival list is context dependent (conjunct or disjunct?*)
let trivial lits =
  let open CCList in
  let pos, neg = partition positive lits in
  let im = Util.image negate neg in
  CCList.inter ~eq:( = ) pos im <> []

let simpdnf fm =
  if fm = False then []
  else if fm = True then [ [] ]
  else
    let djs = CCList.filter (CCFun.negate trivial) (purednf (nnf fm)) in
    (* subset doesn't check for proper but original code checks for proper subset. Does this matter?*)
    CCList.filter
      (fun d ->
        not
          (CCList.exists
             (fun d' -> CCList.subset ~eq:( = ) d' d && d' <> d)
             djs))
      djs

let dnf fm = list_disj (CCList.map list_conj (simpdnf fm))
let purecnf fm = Util.image (Util.image negate) (purednf (nnf (Not fm)))

let simpcnf fm =
  if fm = False then [ [] ]
  else if fm = True then []
  else
    let cjs = CCList.filter (CCFun.negate trivial) (purecnf fm) in
    let isnt_subsumed c =
      not (CCList.exists (fun c' -> Util.psubset c' c) cjs)
    in
    CCList.filter isnt_subsumed cjs

let cnf fm = list_conj (CCList.map list_disj (simpcnf fm))

module Examples = struct
  let ramsey s t n =
    let open CCList in
    let open CCFun in
    let open Util in
    let vertices = 1 -- n in
    let yesgrps = map (Util.allsets 2) (allsets s vertices)
    and nogrps = map (allsets 2) (allsets t vertices) in
    let e = function
      | [ m; n ] -> Atom (P ("p_" ^ string_of_int m ^ "_" ^ string_of_int n))
      | _ -> failwith "Error in ramsey"
    in
    Or
      ( list_disj (map (list_conj % map e) yesgrps),
        list_disj (map (list_conj % map (fun p -> Not (e p))) nogrps) )

  let halfsum x y = Iff (x, Not y)
  let halfcarry x y = And (x, y)
  let ha x y s c = And (Iff (s, halfsum x y), Iff (c, halfcarry x y))
  let carry x y z = Or (And (x, y), And (Or (x, y), z))
  let sum x y z = halfsum (halfsum x y) z
  let fa x y z s c = And (Iff (s, sum x y z), Iff (c, carry x y z))
  let conjoin f l = list_conj (CCList.map f l)

  let ripplecarry x y c out n =
    let open CCList in
    conjoin (fun i -> fa (x i) (y i) (c i) (out i) (c (i + 1))) (0 -- (n - 1))

  let mk_index x i = Atom (P (x ^ "_" ^ string_of_int i))

  and mk_index2 x i j =
    Atom (P (x ^ "_" ^ string_of_int i ^ "_" ^ string_of_int j))

  let ripplecarry0 x y c out n =
    psimplify (ripplecarry x y (fun i -> if i = 0 then False else c i) out n)

  let ripplecarry1 x y c out n =
    psimplify (ripplecarry x y (fun i -> if i = 0 then True else c i) out n)

  let mux sel in0 in1 = Or (And (Not sel, in0), And (sel, in1))
  let offset n x i = x (n + i)

  let rec carryselect x y c0 c1 s0 s1 c s n k =
    let ( -- ) = CCList.( -- ) in
    let k' = min n k in
    let fm =
      And
        ( And (ripplecarry0 x y c0 s0 k', ripplecarry1 x y c1 s1 k'),
          And
            ( Iff (c k', mux (c 0) (c0 k') (c1 k')),
              conjoin
                (fun i -> Iff (s i, mux (c 0) (s0 i) (s1 i)))
                (0 -- (k' - 1)) ) )
    in
    if k' < k then fm
    else
      And
        ( fm,
          carryselect (offset k x) (offset k y) (offset k c0) (offset k c1)
            (offset k s0) (offset k s1) (offset k c) (offset k s) (n - k) k )

  let mk_adder_test n k =
    let open CCList in
    match
      map mk_index [ "x"; "y"; "c"; "s"; "c0"; "s0"; "c1"; "s1"; "c2"; "s2" ]
    with
    | [ x; y; c; s; c0; s0; c1; s1; c2; s2 ] ->
        Imp
          ( And
              ( And (carryselect x y c0 c1 s0 s1 c s n k, Not (c 0)),
                ripplecarry0 x y c2 s2 n ),
            And
              ( Iff (c n, c2 n),
                conjoin (fun i -> Iff (s i, s2 i)) (0 -- (n - 1)) ) )
    | _ -> failwith "Unexpected error"

  let rippleshift u v c z w n =
    ripplecarry0 u v
      (fun i -> if i = n then w (n - 1) else c (i + 1))
      (fun i -> if i = 0 then z else w (i - 1))
      n

  let multiplier x u v out n =
    let ( -- ) = CCList.( -- ) in
    if n = 1 then And (Iff (out 0, x 0 0), Not (out 1))
    else
      psimplify
        (And
           ( Iff (out 0, x 0 0),
             And
               ( rippleshift
                   (fun i -> if i = n - 1 then False else x 0 (i + 1))
                   (x 1) (v 2) (out 1) (u 2) n,
                 if n = 2 then And (Iff (out 2, u 2 0), Iff (out 3, u 2 1))
                 else
                   conjoin
                     (fun k ->
                       rippleshift (u k) (x k)
                         (v (k + 1))
                         (out k)
                         (if k = n - 1 then fun i -> out (n + i) else u (k + 1))
                         n)
                     (2 -- (n - 1)) ) ))

  let rec bitlength x = if x = 0 then 0 else 1 + bitlength (x / 2)
  let rec bit n x = if n = 0 then x mod 2 = 1 else bit (n - 1) (x / 2)

  let congruent_to x m n =
    let open CCList in
    conjoin (fun i -> if bit i m then x i else Not (x i)) (0 -- (n - 1))

  let prime p =
    let open CCList in
    match map mk_index [ "x"; "y"; "out" ] with
    | [ x; y; out ] -> (
        let m i j = And (x i, y j) in
        match map mk_index2 [ "u"; "v" ] with
        | [ u; v ] ->
            let n = bitlength p in
            Not
              (And
                 ( multiplier m u v out (n - 1),
                   congruent_to out p (max n ((2 * n) - 2)) ))
        | _ -> failwith "Bad error")
    | _ -> failwith "Bad error"
end

let mkprop n = (Atom (P ("p_" ^ string_of_int n)), n + 1)

(** example mapping in defs "a || b |-> (p_3, p_3 <=> a || b)",
   i.e. take the formula we are abstracting and map it to the pair of
 its place holder and what it represents*)
let rec maincnf ((fm, _defs, _n) as trip) =
  match fm with
  | And (p, q) -> defstep mk_and (p, q) trip
  | Or (p, q) -> defstep mk_or (p, q) trip
  | Iff (p, q) -> defstep mk_iff (p, q) trip
  | _ -> trip

and defstep op (p, q) (_fm, defs, n) =
  let open Fpf in
  let fm1, defs1, n1 = maincnf (p, defs, n) in
  let fm2, defs2, n2 = maincnf (q, defs1, n1) in
  let fm' = op fm1 fm2 in
  try (fst (apply defs2 fm'), defs2, n2)
  with Failure _ ->
    let v, n3 = mkprop n2 in
    (v, (fm' |-> (v, Iff (v, fm'))) defs2, n3)

let max_varindex pfx =
  let m = String.length pfx in
  fun s n ->
    let l = String.length s in
    if l <= m || String.sub s 0 m <> pfx then n
    else
      let s' = String.sub s m (l - m) in
      if CCList.for_all numeric (CCString.to_list s') then
        max n (int_of_string s')
      else n

let mk_defcnf fn (fm : prop formula) =
  let open CCFun in
  let open Fpf in
  let open CCList in
  let fm' = nenf fm in
  let n = 1 + Formula_core.overatoms (max_varindex "p_" % pname) fm' 0 in
  let fm'', defs, _ = fn (fm', undefined, n) in
  let deflist = map (snd % snd) (graph defs) in
  Util.unions (simpcnf fm'' :: map simpcnf deflist)

(*Old defcnf*)
let defcnf_old fm = list_conj (CCList.map list_disj (mk_defcnf maincnf fm))

let subcnf sfn op ((p, q) : 'a formula * 'a formula) (_fm, defs, n) =
  let fm1, defs1, n1 = sfn (p, defs, n) in
  let fm2, defs2, n2 = sfn (q, defs1, n1) in
  (op fm1 fm2, defs2, n2)

let rec orcnf ((fm, _defs, _n) as trip) =
  match fm with
  | Or (p, q) -> subcnf orcnf mk_or (p, q) trip
  | _ -> maincnf trip

let rec andcnf ((fm, _defs, _n) as trip) =
  match fm with
  | And (p, q) -> subcnf andcnf mk_and (p, q) trip
  | _ -> orcnf trip

let defcnfs fm = mk_defcnf andcnf fm

(* optimized cnf but I don't really understand the implementation*)
let defcnf fm = list_conj (CCList.map list_disj (defcnfs fm))

let rec andcnf3 ((fm, _defs, _n) as trip) =
  match fm with
  | And (p, q) -> subcnf andcnf3 mk_and (p, q) trip
  | _ -> maincnf trip

let defcnf3 fm = list_conj (CCList.map list_disj (mk_defcnf andcnf3 fm))

(*David Putnam  *)
let one_literal_rule clauses =
  let open CCList in
  let u = hd (find (fun cl -> length cl = 1) clauses) in
  let u' = negate u in
  let clauses1 = filter (fun cl -> not (mem u cl)) clauses in
  (*We are subtract the sets below but assuming the the lists have already been setified *)
  Util.image (fun cl -> sorted_diff_uniq ~cmp:Stdlib.compare cl [ u' ]) clauses1

let affirmative_negative_rule clauses =
  let open CCList in
  let neg', pos = partition negative (Util.unions clauses) in
  let neg = Util.image negate neg' in
  let pos_only = sorted_diff_uniq ~cmp:Stdlib.compare pos neg
  and neg_only = sorted_diff_uniq ~cmp:Stdlib.compare neg pos in
  let pure = union ~eq:( = ) pos_only (Util.image negate neg_only) in
  if pure = [] then failwith "affirmative_negative_rule"
  else filter (fun cl -> inter ~eq:( = ) cl pure = []) clauses

let resolve_on p clauses =
  let open CCList in
  let p' = negate p and pos, notpos = partition (mem p) clauses in
  let neg, other = partition (mem p') notpos in
  let pos' = Util.image (filter (fun l -> l <> p)) pos
  and neg' = Util.image (filter (fun l -> l <> p')) neg in
  let res0 = allpairs (union ~eq:( = )) pos' neg' in
  union ~eq:( = ) other (filter (CCFun.negate trivial) res0)

let resolution_blowup cls l =
  let open CCList in
  let m = length (filter (mem l) cls)
  and n = length (filter (mem (negate l)) cls) in
  (m * n) - m - n

let resolution_rule clauses =
  let open CCList in
  let pvs = filter positive (Util.unions clauses) in
  let p = Util.minimize (resolution_blowup clauses) pvs in
  resolve_on p clauses

let rec dp clauses =
  if clauses = [] then true
  else if CCList.mem [] clauses then false
  else
    try
      dp (one_literal_rule clauses)
      (*not found returned by find... ugly to use exceptions, TODO: fix*)
    with Failure _ | Not_found -> (
      try dp (affirmative_negative_rule clauses)
      with Failure _ -> dp (resolution_rule clauses))

let dpsat fm = dp (defcnfs fm)
let dptaut fm = not (dpsat (Not fm))

let posneg_count cls l =
  let open CCList in
  let m = length (filter (mem l) cls)
  and n = length (filter (mem (negate l)) cls) in
  m + n

let rec dpll clauses =
  let open CCList in
  if clauses = [] then true
  else if mem [] clauses then false
  else
    try dpll (one_literal_rule clauses)
    with Failure _ | Not_found -> (
      try dpll (affirmative_negative_rule clauses)
      with Failure _ ->
        let pvs = filter positive (Util.unions clauses) in
        let p = Util.maximize (posneg_count clauses) pvs in
        dpll (sorted_insert ~cmp:Stdlib.compare [ p ] clauses)
        || dpll (sorted_insert ~cmp:Stdlib.compare [ negate p ] clauses))

let dpllsat fm = dpll (defcnfs fm)
let dplltaut fm = not (dpllsat (Not fm))

(*iterative DPLL*)
type trailmix = Guessed | Deduced

let unassigned cls trail =
  let open Util in
  let ( % ) = CCFun.( % ) in
  let litabs p = match p with Not q -> q | _ -> p in
  CCList.sorted_diff_uniq ~cmp:compare
    (unions (image (image litabs) cls))
    (image (litabs % fst) trail)

let rec unit_subpropagate (cls, fn, trail) =
  let open CCList in
  let open Fpf in
  let ( % ) = CCFun.( % ) in
  (*if p is true then we take {{q,r,~p} -> {q,r}, hence why we negate first *)
  let cls' = map (filter (not % defined fn % negate)) cls in
  let uu = function [ c ] when not (defined fn c) -> Some [ c ] | _ -> None in
  let newunits = Util.unions (filter_map uu cls') in
  if newunits = [] then (cls', fn, trail)
  else
    let trail' = fold_right (fun p t -> (p, Deduced) :: t) newunits trail
    and fn' = fold_right (fun u -> u |-> ()) newunits fn in
    unit_subpropagate (cls', fn', trail')

let unit_propagate (cls, trail) =
  let open Fpf in
  let fn = CCList.fold_right (fun (x, _) -> x |-> ()) trail undefined in
  let cls', _fn', trail' = unit_subpropagate (cls, fn, trail) in
  (cls', trail')

let rec backtrack trail =
  match trail with (_p, Deduced) :: tt -> backtrack tt | _ -> trail

let rec dpli cls trail =
  let cls', trail' = unit_propagate (cls, trail) in
  if CCList.mem [] cls' then
    match backtrack trail with
    | (p, Guessed) :: tt -> dpli cls ((negate p, Deduced) :: tt)
    | _ -> false
  else
    match unassigned cls trail' with
    | [] -> true
    | ps ->
        let p = Util.maximize (posneg_count cls') ps in
        dpli cls ((p, Guessed) :: trail')

let dplisat fm = dpli (defcnfs fm) []
let dplitaut fm = not (dplisat (Not fm))

let rec backjump cls p trail =
  let open CCList in
  match backtrack trail with
  | (_q, Guessed) :: tt ->
      let cls', _trail' = unit_propagate (cls, (p, Guessed) :: tt) in
      if mem [] cls' then backjump cls p tt else trail
  | _ -> trail

let rec dplb cls trail =
  let open Util in
  let open CCList in
  let ( % ) = CCFun.( % ) in
  let cls', trail' = unit_propagate (cls, trail) in
  if mem [] cls' then
    match backtrack trail with
    | (p, Guessed) :: tt ->
        let trail' = backjump cls p tt in
        let declits = filter (fun (_, d) -> d = Guessed) trail' in
        let conflict =
          sorted_insert ~cmp:Stdlib.compare (negate p)
            (image (negate % fst) declits)
        in
        dplb (conflict :: cls) ((negate p, Deduced) :: trail')
    | _ -> false
  else
    match unassigned cls trail' with
    | [] -> true
    | ps ->
        let p = maximize (posneg_count cls') ps in
        dplb cls ((p, Guessed) :: trail')

let dplbsat fm = dplb (defcnfs fm) []
let dplbtaut fm = not (dplbsat (Not fm))

(** Basically just turn formula into single literal + list of definitions _ <=> _ x _   *)
let triplicate fm =
  let open Fpf in
  let ( % ) = CCFun.( % ) in
  let fm' = nenf fm in
  let n = 1 + overatoms (max_varindex "p_" % pname) fm' 0 in
  let p, defs, _ = maincnf (fm', undefined, n) in
  (p, CCList.map (snd % snd) (graph defs))

let atom lit = if negative lit then negate lit else lit

let rec align (p, q) =
  if atom p < atom q then align (q, p)
  else if negative p then (negate p, negate q)
  else (p, q)

let equate2 (p, q) eqv =
  Union_find.equate (negate p, negate q) (Union_find.equate (p, q) eqv)

let rec irredundant rel eqs =
  let open Union_find in
  match eqs with
  | [] -> []
  | (p, q) :: oth ->
      if canonize rel p = canonize rel q then irredundant rel oth
      else
        CCList.sorted_insert ~cmp:Stdlib.compare (p, q)
          (irredundant (equate2 (p, q) rel) oth)

let consequences ((p, q) as peq) fm eqs =
  (* ((p <=> q) /\ fm) => (r <=> s) *)
  let follows (r, s) = tautology (Imp (And (Iff (p, q), fm), Iff (r, s))) in
  irredundant (equate2 peq Union_find.unequal) (CCList.filter follows eqs)

(** triggers fm results in a list of (equiv, consequences),
  i.e. if equiv were true, then the list of consequences follows*)
let triggers fm =
  let open CCList in
  let poslits =
    sorted_insert ~cmp:Stdlib.compare True (map (fun p -> Atom p) (atoms fm))
  in
  let lits = union ~eq:( = ) poslits (map negate poslits) in
  let pairs = allpairs (fun p q -> (p, q)) lits lits in
  let npairs = filter (fun (p, q) -> atom p <> atom q) pairs in
  let eqs = sort_uniq ~cmp:Stdlib.compare (map align npairs) in
  let raw = map (fun p -> (p, consequences p fm eqs)) eqs in
  filter (fun (_, c) -> c <> []) raw

let trigger =
  let ( % ) = CCFun.( % ) in
  let ppf = parse_prop_formula in
  let trig_and = triggers @@ ppf {|p <=> q /\ r|} in
  let trig_or = triggers @@ ppf {|p <=> q \/ r|} in
  let trig_imp = triggers @@ ppf {|p <=> q ==> r|} in
  let trig_iff = triggers @@ ppf {|p <=> q <=> r|} in
  let ddnegate fm = match fm with Not (Not p) -> p | _ -> fm in
  let inst_fn = function
    | [ x; y; z ] ->
        let subfn = Fpf.fpf [ P "p"; P "q"; P "r" ] [ x; y; z ] in
        ddnegate % psubst subfn
    | _ -> failwith "unexpected case in inst_fn"
  in
  let inst2_fn i (p, q) = align (inst_fn i p, inst_fn i q) in
  let instn_fn i (a, c) = (inst2_fn i a, CCList.map (inst2_fn i) c) in
  let inst_trigger = CCList.map % instn_fn in
  function
  | Iff (x, And (y, z)) -> inst_trigger [ x; y; z ] trig_and
  | Iff (x, Or (y, z)) -> inst_trigger [ x; y; z ] trig_or
  | Iff (x, Imp (y, z)) -> inst_trigger [ x; y; z ] trig_imp
  | Iff (x, Iff (y, z)) -> inst_trigger [ x; y; z ] trig_iff
  | _ -> failwith "Unexpected case in trigger"

let relevance trigs =
  let open Fpf in
  let insert_relevant p trg f =
    (p |-> CCList.sorted_insert ~cmp:Stdlib.compare trg (tryapplyl f p)) f
  in
  let insert_relevant2 (((p, q), _) as trg) f =
    let f' = insert_relevant q trg f in
    insert_relevant p trg f'
  in
  CCList.fold_right insert_relevant2 trigs undefined

let equatecons (p0, q0) ((eqv, rfn) as erf) =
  let open Union_find in
  let open Fpf in
  let open CCList in
  let ( % ) = CCFun.( % ) in
  let p = canonize eqv p0 and q = canonize eqv q0 in
  if p = q then ([], erf)
  else
    let p' = canonize eqv (negate p0) and q' = canonize eqv (negate q0) in
    let eqv' = equate2 (p, q) eqv
    and sp_pos = tryapplyl rfn p
    and sp_neg = tryapplyl rfn p'
    and sq_pos = tryapplyl rfn q
    and sq_neg = tryapplyl rfn q' in
    let rfn'' =
      let rfn' = (canonize eqv' p' |-> union ~eq:( = ) sp_neg sq_neg) rfn in
      (canonize eqv' p |-> union ~eq:( = ) sp_pos sq_pos) rfn'
    in
    let nw =
      union ~eq:( = )
        (inter ~eq:( = ) sp_pos sq_pos)
        (inter ~eq:( = ) sp_neg sq_neg)
    in
    (fold_right (union ~eq:( = ) % snd) nw [], (eqv', rfn''))

let rec zero_saturate erf assigs =
  match assigs with
  | [] -> erf
  | (p, q) :: ts ->
      let news, erf' = equatecons (p, q) erf in
      zero_saturate erf' (CCList.union ~eq:( = ) ts news)

let zero_saturate_and_check erf trigs =
  let open Union_find in
  let open CCList in
  let ((eqv', _rfn') as erf') = zero_saturate erf trigs in
  let vars = filter positive (equated eqv') in
  if exists (fun x -> canonize eqv' x = canonize eqv' (Not x)) vars then
    snd (equatecons (True, Not True) erf')
  else erf'

let truefalse pfn =
  let open Union_find in
  canonize pfn (Not True) = canonize pfn True

let rec equateset s0 eqfn =
  match s0 with
  | a :: (b :: _s2 as s1) -> equateset s1 (snd (equatecons (a, b) eqfn))
  | _ -> eqfn

let rec inter els ((eq1, _) as erf1) ((eq2, _) as erf2) rev1 rev2 erf =
  let open Union_find in
  let open Fpf in
  match els with
  | [] -> erf
  | x :: xs ->
      let b1 = canonize eq1 x and b2 = canonize eq2 x in
      let s1 = apply rev1 b1 and s2 = apply rev2 b2 in
      let s = CCList.inter ~eq:( = ) s1 s2 in
      inter
        (CCList.sorted_diff_uniq ~cmp:compare xs s)
        erf1 erf2 rev1 rev2 (equateset s erf)

let reverseq domain eqv =
  let open Union_find in
  let open Fpf in
  let al = CCList.map (fun x -> (x, canonize eqv x)) domain in
  CCList.fold_right
    (fun (y, x) f ->
      (x |-> CCList.sorted_insert ~cmp:compare y (tryapplyl f x)) f)
    al undefined

let stal_intersect ((eq1, _) as erf1) ((eq2, _) as erf2) erf =
  let open Union_find in
  if truefalse eq1 then erf2
  else if truefalse eq2 then erf1
  else
    let dom1 = equated eq1 and dom2 = equated eq2 in
    let comdom = CCList.inter ~eq:( = ) dom1 dom2 in
    let rev1 = reverseq dom1 eq1 and rev2 = reverseq dom2 eq2 in
    inter comdom erf1 erf2 rev1 rev2 erf

let rec saturate n erf assigs allvars =
  let ((eqv', _) as erf') = zero_saturate_and_check erf assigs in
  if n = 0 || truefalse eqv' then erf'
  else
    let ((eqv'', _) as erf'') = splits n erf' allvars allvars in
    if eqv'' = eqv' then erf'' else saturate n erf'' [] allvars

and splits n ((eqv, _) as erf) allvars vars =
  let open Union_find in
  match vars with
  | [] -> erf
  | p :: ovars ->
      if canonize eqv p <> p then splits n erf allvars ovars
      else
        let erf0 = saturate (n - 1) erf [ (p, Not True) ] allvars
        and erf1 = saturate (n - 1) erf [ (p, True) ] allvars in
        let ((eqv', _) as erf') = stal_intersect erf0 erf1 erf in
        if truefalse eqv' then erf' else splits n erf' allvars ovars

let rec saturate_upto vars n m trigs assigs =
  let open Union_find in
  if n > m then failwith ("Not " ^ string_of_int m ^ "-easy")
  else (
    print_string ("*** Starting " ^ string_of_int n ^ "-saturation");
    print_newline ();
    let eqv, _ = saturate n (unequal, relevance trigs) assigs vars in
    truefalse eqv || saturate_upto vars (n + 1) m trigs assigs)

let stalmarck fm =
  let open Fpf in
  let open CCList in
  let ( % ) = CCFun.( % ) in
  let include_trig (e, cqs) f = (e |-> union ~eq:( = ) cqs (tryapplyl f e)) f in
  let fm' = psimplify (Not fm) in
  if fm' = False then true
  else if fm' = True then false
  else
    let p, triplets = triplicate fm' in
    let trigfn =
      fold_right (fold_right include_trig % trigger) triplets undefined
    and vars = map (fun p -> Atom p) (Util.unions (map atoms triplets)) in
    saturate_upto vars 0 2 (graph trigfn) [ (p, True) ]
