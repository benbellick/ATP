open Parser_core

type 'a formula =
  | False
  | True
  | Atom of 'a
  | Not of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Imp of 'a formula * 'a formula
  | Iff of 'a formula * 'a formula
  | Forall of string * 'a formula
  | Exists of string * 'a formula

let mk_and p q = And (p, q)
let mk_or p q = Or (p, q)
let mk_iff p q = Iff (p, q)

let rec parse_atomic_formula (ifn, afn) vs inp =
  match inp with
  | [] -> failwith "formula expected"
  | "false" :: rest -> (False, rest)
  | "true" :: rest -> (True, rest)
  | "(" :: rest -> (
      try ifn vs inp
      with Failure _ -> parse_bracketed (parse_formula (ifn, afn) vs) ")" rest)
  | "~" :: rest ->
      papply (fun p -> Not p) (parse_atomic_formula (ifn, afn) vs rest)
  | "forall" :: x :: rest ->
      parse_quant (ifn, afn) (x :: vs) (fun (x, p) -> Forall (x, p)) x rest
  | "exists" :: x :: rest ->
      parse_quant (ifn, afn) (x :: vs) (fun (x, p) -> Exists (x, p)) x rest
  | _ -> afn vs inp

and parse_quant (ifn, afn) vs qcon x inp =
  match inp with
  | [] -> failwith "Body of quantified term expected"
  | y :: rest ->
      papply
        (fun fm -> qcon (x, fm))
        (if y = "." then parse_formula (ifn, afn) vs rest
         else parse_quant (ifn, afn) (y :: vs) qcon y rest)

and parse_formula (ifn, afn) vs inp =
  parse_right_infix "<=>"
    (fun (p, q) -> Iff (p, q))
    (parse_right_infix "==>"
       (fun (p, q) -> Imp (p, q))
       (parse_right_infix "\\/"
          (fun (p, q) -> Or (p, q))
          (parse_right_infix "/\\"
             (fun (p, q) -> And (p, q))
             (parse_atomic_formula (ifn, afn) vs))))
    inp

let rec strip_quant fm =
  match fm with
  | Forall (x, (Forall (_y, _p) as yp)) | Exists (x, (Exists (_y, _p) as yp)) ->
      let xs, q = strip_quant yp in
      (x :: xs, q)
  | Forall (x, p) | Exists (x, p) -> ([ x ], p)
  | _ -> ([], fm)

let print_formula pfn =
  let open Format in
  let rec print_formula pr fm =
    match fm with
    | False -> print_string "false"
    | True -> print_string "true"
    | Atom pargs -> pfn pr pargs
    | Not p -> bracket (pr > 10) 1 (print_prefix 10) "~" p
    | And (p, q) -> bracket (pr > 8) 0 (print_infix 8 "/\\") p q
    | Or (p, q) -> bracket (pr > 6) 0 (print_infix 6 "\\/") p q
    | Imp (p, q) -> bracket (pr > 4) 0 (print_infix 4 "==>") p q
    | Iff (p, q) -> bracket (pr > 2) 0 (print_infix 2 "<=>") p q
    | Forall (_x, _p) -> bracket (pr > 0) 2 print_qnt "forall" (strip_quant fm)
    | Exists (_x, _p) -> bracket (pr > 0) 2 print_qnt "exists" (strip_quant fm)
  and print_qnt qname (bvs, bod) =
    print_string qname;
    do_list
      (fun v ->
        print_string " ";
        print_string v)
      bvs;
    print_string ".";
    print_space ();
    open_box 0;
    print_formula 0 bod;
    close_box ()
  and print_prefix newpr sym p =
    print_string sym;
    print_formula (newpr + 1) p
  and print_infix newpr sym p q =
    print_formula (newpr + 1) p;
    print_string (" " ^ sym);
    print_space ();
    print_formula newpr q
  in
  print_formula 0

(**TODO: fix the abundance of parantheses*)
let rec pp_formula pfn out (fm : 'a formula) =
  let pp = pp_formula pfn in
  let open CCFormat in
  match fm with
  | False -> string out "false"
  | True -> string out "true"
  | Atom a -> fprintf out "%a" pfn a
  | Not a -> fprintf out "~%a" pp a
  | And (p, q) -> fprintf out "(%a /\\ %a)" pp p pp q
  | Or (p, q) -> fprintf out "(%a \\/ %a)" pp p pp q
  | Imp (p, q) -> fprintf out "(%a ==> %a)" pp p pp q
  | Iff (p, q) -> fprintf out "(%a <=> %a)" pp p pp q
  | Forall (p, q) -> fprintf out "(forall %s . %a)" p pp q
  | Exists (p, q) -> fprintf out "(exists %s . %a)" p pp q

let print_qformula pfn fm =
  let open Format in
  open_box 0;
  print_string "<<";
  open_box 0;
  print_formula pfn fm;
  close_box ();
  print_string ">>";
  close_box ()

let pp_qformula pfn out fm =
  let open CCFormat in
  pp_open_box out 0;
  pp_print_string out "<<";
  pp_open_box out 0;
  pp_formula pfn out fm;
  pp_close_box out ();
  pp_print_string out ">>";
  pp_close_box out ()

let rec onatoms f fm =
  match fm with
  | Atom a -> f a
  | Not p -> Not (onatoms f p)
  | And (p, q) -> And (onatoms f p, onatoms f q)
  | Or (p, q) -> Or (onatoms f p, onatoms f q)
  | Imp (p, q) -> Imp (onatoms f p, onatoms f q)
  | Iff (p, q) -> Iff (onatoms f p, onatoms f q)
  | Forall (x, p) -> Forall (x, onatoms f p)
  | Exists (x, p) -> Exists (x, onatoms f p)
  | _ -> fm

let rec overatoms f fm b =
  match fm with
  | Atom a -> f a b
  | Not p -> overatoms f p b
  | And (p, q) | Or (p, q) | Imp (p, q) | Iff (p, q) ->
      overatoms f p (overatoms f q b)
  | Forall (_, p) | Exists (_, p) -> overatoms f p b
  | _ -> b

let atom_union f fm =
  CCList.sort_uniq ~cmp:compare (overatoms (fun h t -> f h @ t) fm [])

let atoms fm = atom_union (fun a -> [ a ]) fm
