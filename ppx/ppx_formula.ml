open Ppxlib

let expand ~loc ~path:_ input =
  Ast_builder.Default.(
    pexp_apply ~loc
      (evar ~loc "Lib.Prop_logic.parse_prop_formula")
      [ (Asttypes.Nolabel, estring ~loc input) ])

let extension =
  Extension.declare "formula" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[ rule ] "prop_transformation"
