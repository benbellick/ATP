let image fn xs =
  let open CCList in
  let vals = map fn xs in
  sort_uniq ~cmp:Stdlib.compare vals

let psubset l1 l2 =
  CCList.subset ~eq:(=) l1 l2 && l1 <> l2
                                 
let ramsey s t n =
let vertices = 1 -- n in
let yesgrps = CCList.map (allsets 2) (allsets s vertices)
and nogrps = CCList.map (allsets 2) (allsets t vertices) in
let e[m;n] = Atom(P("p_"^(string_of_int m)^"_"^(string_of_int n))) in
Or(list_disj (map (list_conj ** map e) yesgrps),
list_disj (map (list_conj ** map (fun p -> Not(e p))) nogrps));;
