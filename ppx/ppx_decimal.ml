open Ppxlib
open Ast_builder.Default

(* TODO: add some support for setting context variables. Right now we just use the default *)
let mk_of_string ~loc =
  pexp_ident ~loc { txt = Ldot (Lident "Decimal", "of_string"); loc }

let expand loc str =
  pexp_apply ~loc (mk_of_string ~loc)
    [Nolabel, pexp_constant ~loc @@ Pconst_string (str, loc, None)]

let int_rule = Context_free.Rule.constant Integer 'r' expand
let float_rule = Context_free.Rule.constant Float 'r' expand

let () = Driver.V2.register_transformation "ppx-decimal" ~rules: [ int_rule; float_rule ]
