open Ppxlib

let expand ~loc ~path:_ _ = [%expr "hey"]

let extension =
  Extension.declare
    "formula"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand
    
let rule = Context_free.Rule.extension extension

let () =
  Driver.register_transformation ~rules:[rule] "prop_transformation"
