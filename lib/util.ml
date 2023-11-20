let image fn xs =
  let open CCList in
  let vals = map fn xs in
  sort_uniq ~cmp:Stdlib.compare vals

let psubset l1 l2 = CCList.subset ~eq:( = ) l1 l2 && l1 <> l2

let rec allsets m l =
  if m = 0 then [ [] ]
  else
    match l with
    | [] -> []
    | h :: t ->
        CCList.union ~eq:( = )
          (image (fun g -> h :: g) (allsets (m - 1) t))
          (allsets m t)

let unions (sets : 'a list list) =
  let union (l1 : 'a list) (l2 : 'a list) = CCList.union ~eq:( = ) l1 l2 in
  let reduce (ss : 'a list list) = CCList.reduce_exn union ss in
  CCList.sort_uniq ~cmp:compare (reduce sets)
