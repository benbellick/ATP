open Lib
open Formula_core
open Prop_logic

(** 2.1 *)
let gen_n_prop_formsdepth depth =
  let map_all_pairs fn xs =
    let open CCList in
    let* x1 = xs in
    let+ x2 = xs in
    fn x1 x2
  in
  let map_all_singles fn xs =
    let open CCList in
    let+ x = xs in
    fn x
  in
  let gen_fns =
    map_all_singles mk_not
    :: CCList.map map_all_pairs [ mk_and; mk_or; mk_imp; mk_iff ]
  in
  let rec aux_fn = function
    | depth when depth < 0 -> failwith "Cannot gen negative sized numbers"
    | 0 -> [ True; False ]
    | depth ->
        let prop = Atom (P ("p_" ^ string_of_int depth)) in
        let bricks = prop :: aux_fn (depth - 1) in
        CCList.flat_map (fun f -> f bricks) gen_fns
  in
  aux_fn depth
