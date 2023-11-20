(* ------------------------------------------------------------------------- *)
(* Polymorphic finite partial functions via Patricia trees.                  *)
(*                                                                           *)
(* The point of this strange representation is that it is canonical (equal   *)
(* functions have the same encoding) yet reasonably efficient on average.    *)
(*                                                                           *)
(* Idea due to Diego Olivier Fernandez Pons (OCaml list, 2003/11/10).        *)
(* ------------------------------------------------------------------------- *)
type ('a, 'b) func =
  | Empty
  | Leaf of int * ('a * 'b) list
  | Branch of int * int * ('a, 'b) func * ('a, 'b) func

(* ------------------------------------------------------------------------- *)
(* Undefined function.                                                       *)
(* ------------------------------------------------------------------------- *)

let undefined = Empty

(* ------------------------------------------------------------------------- *)
(* In case of equality comparison worries, better use this.                  *)
(* ------------------------------------------------------------------------- *)

let is_undefined f = match f with Empty -> true | _ -> false

(* ------------------------------------------------------------------------- *)
(* Operation analogous to "map" for lists.                                   *)
(* ------------------------------------------------------------------------- *)

let mapf =
  let rec map_list f l =
    match l with [] -> [] | (x, y) :: t -> (x, f y) :: map_list f t
  in
  let rec mapf f t =
    match t with
    | Empty -> Empty
    | Leaf (h, l) -> Leaf (h, map_list f l)
    | Branch (p, b, l, r) -> Branch (p, b, mapf f l, mapf f r)
  in
  mapf

(* ------------------------------------------------------------------------- *)
(* Operations analogous to "fold" for lists.                                 *)
(* ------------------------------------------------------------------------- *)

let foldl =
  let rec foldl_list f a l =
    match l with [] -> a | (x, y) :: t -> foldl_list f (f a x y) t
  in
  let rec foldl f a t =
    match t with
    | Empty -> a
    | Leaf (_h, l) -> foldl_list f a l
    | Branch (_p, _b, l, r) -> foldl f (foldl f a l) r
  in
  foldl

let foldr =
  let rec foldr_list f l a =
    match l with [] -> a | (x, y) :: t -> f x y (foldr_list f t a)
  in
  let rec foldr f t a =
    match t with
    | Empty -> a
    | Leaf (_h, l) -> foldr_list f l a
    | Branch (_p, _b, l, r) -> foldr f l (foldr f r a)
  in
  foldr

(* ------------------------------------------------------------------------- *)
(* Mapping to sorted-list representation of the graph, domain and range.     *)
(* ------------------------------------------------------------------------- *)

let graph f =
  CCList.sort_uniq ~cmp:compare (foldl (fun a x y -> (x, y) :: a) [] f)

let dom f = CCList.sort_uniq ~cmp:compare (foldl (fun a x _y -> x :: a) [] f)
let ran f = CCList.sort_uniq ~cmp:compare (foldl (fun a _x y -> y :: a) [] f)

(* ------------------------------------------------------------------------- *)
(* Application.                                                              *)
(* ------------------------------------------------------------------------- *)

let applyd =
  let rec apply_listd l d x =
    match l with
    | (a, b) :: t ->
        let c = compare x a in
        if c = 0 then b else if c > 0 then apply_listd t d x else d x
    | [] -> d x
  in
  fun f d x ->
    let k = Hashtbl.hash x in
    let rec look t =
      match t with
      | Leaf (h, l) when h = k -> apply_listd l d x
      | Branch (p, b, l, r) when k lxor p land (b - 1) = 0 ->
          look (if k land b = 0 then l else r)
      | _ -> d x
    in
    look f

let apply f = applyd f (fun _x -> failwith "apply")
let tryapplyd f a d = applyd f (fun _x -> d) a
let tryapplyl f x = tryapplyd f x []

let defined f x =
  try
    apply f x;
    true
  with Failure _ -> false

(* ------------------------------------------------------------------------- *)
(* Undefinition.                                                             *)
(* ------------------------------------------------------------------------- *)

let undefine =
  let rec undefine_list x l =
    match l with
    | ((a, _b) as ab) :: t ->
        let c = compare x a in
        if c = 0 then t
        else if c < 0 then l
        else
          let t' = undefine_list x t in
          if t' == t then l else ab :: t'
    | [] -> []
  in
  fun x ->
    let k = Hashtbl.hash x in
    let rec und t =
      match t with
      | Leaf (h, l) when h = k ->
          let l' = undefine_list x l in
          if l' == l then t else if l' = [] then Empty else Leaf (h, l')
      | Branch (p, b, l, r) when k land (b - 1) = p -> (
          if k land b = 0 then
            let l' = und l in
            if l' == l then t
            else match l' with Empty -> r | _ -> Branch (p, b, l', r)
          else
            let r' = und r in
            if r' == r then t
            else match r' with Empty -> l | _ -> Branch (p, b, l, r'))
      | _ -> t
    in
    und

(* ------------------------------------------------------------------------- *)
(* Redefinition and combination.                                             *)
(* ------------------------------------------------------------------------- *)

let ( |-> ), combine =
  let newbranch p1 t1 p2 t2 =
    let zp = p1 lxor p2 in
    let b = zp land -zp in
    let p = p1 land (b - 1) in
    if p1 land b = 0 then Branch (p, b, t1, t2) else Branch (p, b, t2, t1)
  in
  let rec define_list ((x, _y) as xy) l =
    match l with
    | ((a, _b) as ab) :: t ->
        let c = compare x a in
        if c = 0 then xy :: t
        else if c < 0 then xy :: l
        else ab :: define_list xy t
    | [] -> [ xy ]
  and combine_list op z l1 l2 =
    match (l1, l2) with
    | [], _ -> l2
    | _, [] -> l1
    | ((x1, y1) as xy1) :: t1, ((x2, y2) as xy2) :: t2 ->
        let c = compare x1 x2 in
        if c < 0 then xy1 :: combine_list op z t1 l2
        else if c > 0 then xy2 :: combine_list op z l1 t2
        else
          let y = op y1 y2 and l = combine_list op z t1 t2 in
          if z y then l else (x1, y) :: l
  in
  let ( |-> ) x y =
    let k = Hashtbl.hash x in
    let rec upd t =
      match t with
      | Empty -> Leaf (k, [ (x, y) ])
      | Leaf (h, l) ->
          if h = k then Leaf (h, define_list (x, y) l)
          else newbranch h t k (Leaf (k, [ (x, y) ]))
      | Branch (p, b, l, r) ->
          if k land (b - 1) <> p then newbranch p t k (Leaf (k, [ (x, y) ]))
          else if k land b = 0 then Branch (p, b, upd l, r)
          else Branch (p, b, l, upd r)
    in
    upd
  in
  let rec combine op z t1 t2 =
    match (t1, t2) with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Leaf (h1, l1), Leaf (h2, l2) ->
        if h1 = h2 then
          let l = combine_list op z l1 l2 in
          if l = [] then Empty else Leaf (h1, l)
        else newbranch h1 t1 h2 t2
    | (Leaf (k, _lis) as lf), (Branch (p, b, l, r) as br) ->
        if k land (b - 1) = p then
          if k land b = 0 then
            match combine op z lf l with
            | Empty -> r
            | l' -> Branch (p, b, l', r)
          else
            match combine op z lf r with
            | Empty -> l
            | r' -> Branch (p, b, l, r')
        else newbranch k lf p br
    | (Branch (p, b, l, r) as br), (Leaf (k, _lis) as lf) ->
        if k land (b - 1) = p then
          if k land b = 0 then
            match combine op z l lf with
            | Empty -> r
            | l' -> Branch (p, b, l', r)
          else
            match combine op z r lf with
            | Empty -> l
            | r' -> Branch (p, b, l, r')
        else newbranch p br k lf
    | Branch (p1, b1, l1, r1), Branch (p2, b2, l2, r2) ->
        if b1 < b2 then
          if p2 land (b1 - 1) <> p1 then newbranch p1 t1 p2 t2
          else if p2 land b1 = 0 then
            match combine op z l1 t2 with
            | Empty -> r1
            | l -> Branch (p1, b1, l, r1)
          else
            match combine op z r1 t2 with
            | Empty -> l1
            | r -> Branch (p1, b1, l1, r)
        else if b2 < b1 then
          if p1 land (b2 - 1) <> p2 then newbranch p1 t1 p2 t2
          else if p1 land b2 = 0 then
            match combine op z t1 l2 with
            | Empty -> r2
            | l -> Branch (p2, b2, l, r2)
          else
            match combine op z t1 r2 with
            | Empty -> l2
            | r -> Branch (p2, b2, l2, r)
        else if p1 = p2 then
          match (combine op z l1 l2, combine op z r1 r2) with
          | Empty, r -> r
          | l, Empty -> l
          | l, r -> Branch (p1, b1, l, r)
        else newbranch p1 t1 p2 t2
  in
  (( |-> ), combine)

(* ------------------------------------------------------------------------- *)
(* Special case of point function.                                           *)
(* ------------------------------------------------------------------------- *)

let ( |=> ) x y = (x |-> y) undefined

(* ------------------------------------------------------------------------- *)
(* Idiom for a mapping zipping domain and range lists.                       *)
(* ------------------------------------------------------------------------- *)

let fpf xs ys = CCList.fold_right2 ( |-> ) xs ys undefined

(* ------------------------------------------------------------------------- *)
(* Grab an arbitrary element.                                                *)
(* ------------------------------------------------------------------------- *)

let rec choose t =
  match t with
  | Empty -> failwith "choose: completely undefined function"
  | Leaf (_h, l) -> List.hd l
  | Branch (_b, _p, t1, _t2) -> choose t1

(* ------------------------------------------------------------------------- *)
(* Install a (trivial) printer for finite partial functions.                 *)
(* ------------------------------------------------------------------------- *)

let print_fpf (_f : ('a, 'b) func) = print_string "<func>"

(*#install_printer print_fpf;;*)
