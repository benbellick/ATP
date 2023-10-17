open Lib.Prop_logic

let s = [%formula "p /\\ q ==> p \\/ q"]
let () = print_truthtable s

