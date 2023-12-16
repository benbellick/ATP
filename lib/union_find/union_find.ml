open Fpf
(* ------------------------------------------------------------------------- *)
(* Union-find algorithm.                                                     *)
(* ------------------------------------------------------------------------- *)

type 'a pnode = Nonterminal of 'a | Terminal of 'a * int
type 'a partition = Partition of ('a, 'a pnode) func

let rec terminus (Partition f as ptn) a =
  match apply f a with
  | Nonterminal b -> terminus ptn b
  | Terminal (p, q) -> (p, q)

let tryterminus ptn a = try terminus ptn a with Failure _ -> (a, 1)
let canonize ptn a = fst (tryterminus ptn a)
let equivalent eqv a b = canonize eqv a = canonize eqv b

let equate (a, b) (Partition f as ptn) =
  let a', na = tryterminus ptn a and b', nb = tryterminus ptn b in
  Partition
    (if a' = b' then f
     else if na <= nb then
       CCList.fold_right CCFun.id
         [ a' |-> Nonterminal b'; b' |-> Terminal (b', na + nb) ]
         f
     else
       CCList.fold_right CCFun.id
         [ b' |-> Nonterminal a'; a' |-> Terminal (a', na + nb) ]
         f)

let unequal = Partition undefined
let equated (Partition f) = dom f
