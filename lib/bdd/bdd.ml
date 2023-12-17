open Prop_logic
open Fpf

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
  compare : prop -> prop -> bool;
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
let lookup_unique ({ unique; expand; compare; min_unused_n_idx = n } as bdd)
    node =
  try (bdd, apply unique node)
  with Failure _ ->
    ( {
        unique = (node |-> n) unique;
        expand = (n |-> node) expand;
        min_unused_n_idx = n + 1;
        compare;
      },
      n )

let mk_node bdd node =
  if node.left = node.right then (bdd, node.left)
  else if node.left >= 0 then lookup_unique bdd node
  else
    let bdd', n = lookup_unique bdd (negate_node node) in
    (bdd', -n)

let mk_bdd ord =
  {
    unique = undefined;
    expand = undefined;
    min_unused_n_idx = 2;
    compare = ord;
  }

let order bdd p1 p2 = (p2 = P "" && p1 <> P "") || bdd.compare p1 p2

let thread s g (f1, x1) (f2, x2) =
  let s', y1 = f1 s x1 in
  let s'', y2 = f2 s' x2 in
  g s'' (y1, y2)

let rec bdd_and ((bdd, comp) as bddcomp) (m1, m2) =
  if m1 = -1 || m2 = -1 then (bddcomp, -1)
  else if m1 = 1 then (bddcomp, m2)
  else if m2 = 1 then (bddcomp, m1)
  else
    try (bddcomp, apply comp (m1, m2))
    with Failure _ -> (
      try (bddcomp, apply comp (m2, m1))
      with Failure _ ->
        let { prop = p1; left = l1; right = r1 } = expand_node bdd m1
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
