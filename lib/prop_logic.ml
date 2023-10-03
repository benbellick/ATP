(* type ('a)formula = False *)
(*                  | True *)
(*                  | Atom of 'a *)
(*                  | Not of ('a)formula *)
(*                  | And of ('a)formula * ('a)formula *)
(*                  | Or of ('a)formula * ('a)formula *)
(*                  | Imp of ('a)formula * ('a)formula *)
(*                  | Iff of ('a)formula * ('a)formula *)
(*                  | Forall of string * ('a)formula *)
(*                  | Exists of string * ('a)formula *)

(* type prop = P of string *)

(* let pname(P s) = s *)

(* let parse_propvar _vs inp = *)
(*   match inp with *)
(*   | p::oinp when p <> "(" -> Atom(P(p)),oinp *)
(*   | _ -> failwith "parse_propvar"  *)
