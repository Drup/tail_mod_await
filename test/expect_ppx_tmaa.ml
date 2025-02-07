open Ppxlib
module Ast = Ast_builder.Default

(** Utility for AST equality test-cases. *)

(* All locations are none, we will erase them. *)
let loc = Location.none

let simplifier = object
  inherit Ast_traverse.map
  (* Erase locations. *)
  method! location _ = Location.none

  (* Cleanup identifiers. *)
  method! longident = function
    | Ldot (Ldot ( Lident "Ppx_tmaa_runtime", s1), s2) ->
      Ldot (Lident s1, s2)
    | lid -> lid
end

let expr_testable =
  let pp fmt e = Pprintast.expression fmt (simplifier#expression e) in
  let equal expr1 expr2 =
    simplifier#expression expr1 = simplifier#expression expr2
  in 
  Alcotest.testable pp equal

(* Rewrite an expressions in DPS using the destination "dest". *)
let rewrite_expr e =
  let modname = lident "A" in
  let dps_funs = Ppx_tmaa.DpsFuns.empty in
  let dest = Ast.evar ~loc "dest" in
  Ppx_tmaa.reset_ids ();
  (Ppx_tmaa.rewrite_dps modname dps_funs)#expression dest e

let check_expr (s, e1, e2) =
  let open Alcotest in
  test_case s `Quick (fun () -> check expr_testable s (rewrite_expr e1) e2)

(** Tests over rewritten expressions. *)
let expr_tests = List.map check_expr [
    "constant",
    [%expr 3],
    [%expr Destiny.fill dest 3];
    
    "complicated constant",
    [%expr Foo (`Bar, 15, [| Bar |], {x=3})],
    [%expr
      let v__001_ = [|Bar|] in
      let v__002_ = { x = 3 } in
      let v__003_ = Foo (`Bar, 15, v__001_, v__002_) in
      Destiny.fill dest v__003_
    ];

    "constant constructor",
    [%expr A],
    [%expr
      Destiny.fill dest A
    ];
    "1-constructor",
    [%expr A (A.await e)],
    [%expr
       let [dest__002_] =
         let v__001_ = A Loc.dummy in
         Destiny.refine dest v__001_ [Loc.mk v__001_ 0]
       in
       Destiny.chain e dest__002_
    ];
    "tuple-constructor",
    [%expr A (A.await e1, 4, A.await e2, 5)],
    [%expr
       let [dest__002_; dest__003_] =
         let v__001_ = A (Loc.dummy, 4, Loc.dummy, 5) in
         Destiny.refine dest v__001_ [Loc.mk v__001_ 0; Loc.mk v__001_ 2]
       in
       Destiny.chain e2 dest__003_;
       Destiny.chain e1 dest__002_
    ];

    "constant polyvar",
    [%expr `A],
    [%expr
      Destiny.fill dest `A
    ];
    "1-polyvar",
    [%expr `A (A.await e)],
    [%expr
       let [dest__002_] =
         let v__001_ = `A Loc.dummy in
         Destiny.refine dest v__001_ [Loc.mk v__001_ 1]
       in
       Destiny.chain e dest__002_
    ];
    "tuple-polyvar",
    [%expr `A (A.await e1, 4, A.await e2, 5)],
    [%expr
      let [dest__002_; dest__003_] =
        let v__001_ = (Loc.dummy, 4, Loc.dummy, 5) in
        let v__004_ = `A v__001_ in
        Destiny.refine dest v__004_ [Loc.mk v__001_ 0; Loc.mk v__001_ 2]
      in
      Destiny.chain e2 dest__003_; Destiny.chain e1 dest__002_
    ];
  ]

(** Test runner. *)

let () =
  Alcotest.run "Ppx_tmaa" [
    "Expression equivalence", expr_tests
  ]
