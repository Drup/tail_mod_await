open Ppxlib
module Ast = Ast_builder.Default

(** Utility for AST equality test-cases. *)

(* All locations are none, we will erase them. *)
let loc = Location.none
let loc_eraser = object
  inherit Ast_traverse.map
  method! location _ = Location.none
end

let expr_testable =
  let pp = Pprintast.expression in
  let equal expr1 expr2 =
    loc_eraser#expression expr1 = loc_eraser#expression expr2
  in 
  Alcotest.testable pp equal

(* Rewrite an expressions in DPS using the destination "dest". *)
let rewrite_expr e =
  let modname = Loc.make ~loc (Longident.Lident "Async") in
  let dps_funs = Ppx_tmaa.DpsFuns.empty in
  let dest = Ast.evar ~loc "dest" in
  (Ppx_tmaa.rewrite_dps modname dps_funs)#expression dest e

let check_expr (s, e1, e2) =
  let open Alcotest in
  test_case s `Quick (fun () -> check expr_testable s (rewrite_expr e1) e2)

(** Tests over rewritten expressions. *)
let expr_tests = List.map check_expr [
    "constant",
    [%expr 3],
    [%expr Ppx_tmaa_runtime.Destiny.fill dest 3];

    "constant constructor",
    [%expr A],
    [%expr
      let [] = Ppx_tmaa_runtime.Destiny.refine dest A [] in ()
    ];  
  ]

(** Test runner. *)

let () =
  Alcotest.run "Ppx_tmaa" [
    "Expression equivalence", expr_tests
  ]
