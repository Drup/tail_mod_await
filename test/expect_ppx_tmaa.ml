open Ppxlib
module Ast = Ast_builder.Default

let loc_eraser = object
  inherit Ast_traverse.map
  method! location _ = Location.none
end

let pp = Pprintast.expression
let equal expr1 expr2 =
  loc_eraser#expression expr1 = loc_eraser#expression expr2
let ast = Alcotest.testable pp equal

let loc = Location.none
let expr_rewrite e =
  let modname = Loc.make ~loc (Longident.Lident "Async") in
  let dps_funs = Ppx_tmaa.DpsFuns.empty in
  let dest = Ast.evar ~loc "dest" in
  (Ppx_tmaa.rewrite_dps modname dps_funs)#expression dest e

let mk (s, e1, e2) =
  let open Alcotest in
  test_case s `Quick (fun () -> check ast s (expr_rewrite e1) e2)

let expr_tests = [
  "constant",
  [%expr 3],
  [%expr Ppx_tmaa_runtime.Destiny.fill dest 3];

  "constant constructor",
  [%expr A],
  [%expr
    let [] = Ppx_tmaa_runtime.Destiny.refine dest A [] in ()
  ];  
]

let () =
  Alcotest.run "Ppx_tmaa" [
    "Expression equivalence", List.map mk expr_tests
  ]
