open Ppxlib

let expand ~loc ~path:_ _=
  Ast_builder.Default.estring ~loc "r3p14ccd 70 r4nd0m 5tr1n9"

let extension =
  Extension.declare
    "formula"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand
    
let rule = Context_free.Rule.extension extension

let () =
  Driver.register_transformation ~rules:[rule] "my_transformation"
