open Ppxlib

let kind = Context_free.Rule.Constant_kind.Integer

let rewriter loc s =
  let value =
    Ast_builder.Default.pexp_constant ~loc (Parsetree.Pconst_integer (s, None))
  in
  [%expr Ints.Uint32.of_int [%e value]]

let rule = Context_free.Rule.constant kind 'u' rewriter
let () = Driver.register_transformation ~rules:[ rule ] "constant"
