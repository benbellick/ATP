open Lib.Prop_logic

let ps =
  [ [%formula "p /\\ q ==> p \\/ q"]; [%formula "p /\\ q \\/ ~( p \\/ q)"] ]

let rec f = function
  | [] -> ()
  | p :: ps ->
      let open Format in
      print_prop_formula p;
      print_newline ();
      print_truthtable p;
      print_newline ();
      (* print_prop_formula @@ dual p; *)
      print_newline ();
      print_prop_formula @@ psimplify p;
      print_newline ();
      print_prop_formula @@ dnf p;
      f ps

let () = f ps
