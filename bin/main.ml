open Lib

let s = "p /\\ q ==> p \\/ q"
let fm = Prop_logic.parse_prop_formula s
let () = Prop_logic.print_truthtable fm
