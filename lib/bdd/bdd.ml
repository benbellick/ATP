open Prop_logic
open Fpf
open Formula_core

type bddnode = {
  prop : prop;
  left : int;  (** left subtree node idx *)
  right : int;  (** right subtree node idx *)
}
(**
   I am taking some liberties
   here to reinterpret the types John uses.
   Maybe records didn't exist then? *)

type bdd = {
  unique : (bddnode, int) func;  (** Associate nodes to their idx *)
  expand : (int, bddnode) func;  (** Associate an idx to their node *)
  min_unused_n_idx : int;
  ord : prop -> prop -> bool;  (** e.g. (<)  *)
}

let print_bdd { min_unused_n_idx; _ } =
  print_string ("<BDD with " ^ string_of_int min_unused_n_idx ^ " nodes>")

let negate_node { prop; left; right } = { prop; left = -left; right = -right }

let expand_node { expand; _ } n =
  if n >= 0 then tryapplyd expand n { prop = P ""; left = 1; right = 1 }
  else
    let node = tryapplyd expand (-n) { prop = P ""; left = 1; right = 1 } in
    negate_node node

(** Given a bdd and a node, find its index if it exists, and return the
    bdd and the idx. Otherwise, return the bdd with the node inserted as well as the index
    it was inserted at*)
let lookup_unique ({ unique; expand; ord; min_unused_n_idx = n } as bdd) node =
  try (bdd, apply unique node)
  with Failure _ ->
    ( {
        unique = (node |-> n) unique;
        expand = (n |-> node) expand;
        min_unused_n_idx = n + 1;
        ord;
      },
      n )

let mk_node bdd node =
  if node.left = node.right then (bdd, node.left)
  else if node.left >= 0 then lookup_unique bdd node
  else
    let bdd', n = lookup_unique bdd (negate_node node) in
    (bdd', -n)

let mk_bdd ord =
  { unique = undefined; expand = undefined; min_unused_n_idx = 2; ord }

let order bdd p1 p2 = (p2 = P "" && p1 <> P "") || bdd.ord p1 p2

let thread s g (f1, x1) (f2, x2) =
  let s', y1 = f1 s x1 in
  let s'', y2 = f2 s' x2 in
  g s'' (y1, y2)

let rec bdd_and ((bdd, comp) as bddcomp) (m1, m2) =
  (* Recall that -1/1 means a valuation of false/true *)
  if m1 = -1 || m2 = -1 then (bddcomp, -1)
  else if m1 = 1 then (bddcomp, m2)
  else if m2 = 1 then (bddcomp, m1)
  else
    (* this is now the case where they are both non-trivial *)
    try (bddcomp, apply comp (m1, m2))
    with Failure _ -> (
      try (bddcomp, apply comp (m2, m1))
      with Failure _ ->
        let { prop = p1; left = l1; right = r1 } = expand_node bdd m1
        (* picture of what we are evaluating:
                 _|_
                /   \
             (m1)   (m2)
             p_1    p_2
            /  \   /   \
           l1  r1 l2   r2 *)
        and { prop = p2; left = l2; right = r2 } = expand_node bdd m2 in
        let p, lpair, rpair =
          if p1 = p2 then (p1, (l1, l2), (r1, r2))
          else if order bdd p1 p2 then (p1, (l1, m2), (r1, m2))
          else (p2, (m1, l2), (m1, r2))
        in
        let (bdd', comp'), (lnew, rnew) =
          thread bddcomp (fun s z -> (s, z)) (bdd_and, lpair) (bdd_and, rpair)
        in
        let bdd'', n = mk_node bdd' { prop = p; left = lnew; right = rnew } in
        ((bdd'', ((m1, m2) |-> n) comp'), n))

let bdd_or bdc (m1, m2) =
  (* p \/ q == ~(~p /\ ~q) *)
  let bdc1, n = bdd_and bdc (-m1, -m2) in
  (bdc1, -n)

let bdd_imp bdc (m1, m2) =
  (* p ==> q == ~p \/ q *)
  bdd_or bdc (-m1, m2)

let bdd_iff bdc (m1, m2) =
  (* p <=> q == (p /\ q) \/ (~p /\ ~q) *)
  thread bdc bdd_or (bdd_and, (m1, m2)) (bdd_and, (-m1, -m2))

let rec mkbdd ((bdd, comp) as bddcomp) fm =
  match fm with
  | False -> (bddcomp, -1)
  | True -> (bddcomp, 1)
  | Atom prop ->
      let bdd', n = mk_node bdd { prop; left = 1; right = -1 } in
      ((bdd', comp), n)
  | Not p ->
      let bddcomp', n = mkbdd bddcomp p in
      (bddcomp', -n)
  | And (p, q) -> thread bddcomp bdd_and (mkbdd, p) (mkbdd, q)
  | Or (p, q) -> thread bddcomp bdd_or (mkbdd, p) (mkbdd, q)
  | Imp (p, q) -> thread bddcomp bdd_imp (mkbdd, p) (mkbdd, q)
  | Iff (p, q) -> thread bddcomp bdd_iff (mkbdd, p) (mkbdd, q)
  | _ -> failwith "unhandled quantifiers reached"

let bddtaut fm = snd (mkbdd (mk_bdd ( < ), undefined) fm) = 1
let dest_nimp fm = match fm with Not p -> (p, False) | _ -> dest_imp fm

(** I Can no longer live with exception handling as control flow...
instead of his imple, lets use option*)
let dest_iffdef fm =
  match fm with Iff (Atom x, r) | Iff (r, Atom x) -> Some (x, r) | _ -> None

let restore_iffdef (x, e) fm = Imp (Iff (Atom x, e), fm)

let suitable_iffdef (defs : ('a * 'a formula) list) (_x, q) =
  let open CCList in
  let fvs = atoms q in
  not (exists (fun (x', _) -> mem x' fvs) defs)

let rec sort_defs acc defs fm =
  let open CCList in
  try
    let x, e = find (suitable_iffdef defs) defs in
    let ps, nonps = partition (fun (x', _) -> x' = x) defs in
    let ps' = sorted_diff_uniq ~cmp:Stdlib.compare ps [ (x, e) ] in
    sort_defs ((x, e) :: acc) nonps (fold_right restore_iffdef ps' fm)
  with Failure _ -> (rev acc, fold_right restore_iffdef defs fm)

let rec mkbdde sfn ((bdd, comp) as bddcomp) fm =
  match fm with
  | False -> (bddcomp, -1)
  | True -> (bddcomp, 1)
  | Atom s -> (
      try (bddcomp, apply sfn s)
      with Failure _ ->
        let bdd', n = mk_node bdd { prop = s; left = 1; right = -1 } in
        ((bdd', comp), n))
  | Not p ->
      let bddcomp', n = mkbdde sfn bddcomp p in
      (bddcomp', -n)
  | And (p, q) -> thread bddcomp bdd_and (mkbdde sfn, p) (mkbdde sfn, q)
  | Or (p, q) -> thread bddcomp bdd_or (mkbdde sfn, p) (mkbdde sfn, q)
  | Imp (p, q) -> thread bddcomp bdd_imp (mkbdde sfn, p) (mkbdde sfn, q)
  | Iff (p, q) -> thread bddcomp bdd_iff (mkbdde sfn, p) (mkbdde sfn, q)
  | _ -> failwith "Unhandled quantifiers"

let rec mkbdds sfn bdd defs fm =
  match defs with
  | [] -> mkbdde sfn bdd fm
  | (p, e) :: odefs ->
      let bdd', b = mkbdde sfn bdd e in
      mkbdds ((p |-> b) sfn) bdd' odefs fm

let ebddtaut fm =
  let open CCList in
  let ( % ) = CCFun.( % ) in
  let l, r = try dest_nimp fm with Failure _ -> (True, fm) in
  let eqs, noneqs = partition (CCOption.is_some % dest_iffdef) (conjuncts l) in
  let defs, fm' =
    sort_defs []
      (map (CCOption.get_exn_or "Encountered non iffdef" % dest_iffdef) eqs)
      (fold_right mk_imp noneqs r)
  in
  snd (mkbdds undefined (mk_bdd ( < ), undefined) defs fm') = 1
