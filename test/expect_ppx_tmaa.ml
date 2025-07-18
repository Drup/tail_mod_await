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
  method! location_stack _ = []
end

let expr_testable =
  let pp fmt e =
    (* Pp_ast.expression fmt (simplifier#expression e) *)
    Pprintast.expression fmt (simplifier#expression e)
  in
  let equal expr1 expr2 =
    simplifier#expression expr1 = simplifier#expression expr2
  in 
  Alcotest.testable pp equal

(* Rewrite an expressions in DPS using the destination "dest". *)
let rewrite_expr e =
  let modname = lident "A" in
  let dps_funs = Ppx_tmaa.DpsFuns.singleton "f" ({loc; txt = "f_dps"}) in
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
      let v__002_ = [|Bar|] in
      let v__004_ = { x = 3 } in
      let v__009_ = Foo (`Bar, 15, v__002_, v__004_) in
      Destiny.fill dest v__009_
    ];

    "constant constructor",
    [%expr A],
    [%expr
      Destiny.fill dest A
    ];
    "1-constructor",
    [%expr A (A.await e)],
    [%expr
      let dest__003_::[] =
        let v__002_ = A (Loc.dummy () : 't___001_) in
        Destiny.refine dest v__002_
          [(Loc.mk v__002_ 0 : 't___001_ Loc.t)] in
      Destiny.chain e dest__003_
    ];
    "tuple-constructor",
    [%expr A (A.await e1, 4, A.await e2, 5)],
    [%expr
      let dest__006_::dest__007_::[] =
        let v__005_ =
          A ((Loc.dummy () : 't___001_), 4, (Loc.dummy () : 't___003_), 5)
        in
        Destiny.refine dest v__005_
          [(Loc.mk v__005_ 0 : 't___001_ Loc.t);
           (Loc.mk v__005_ 2 : 't___003_ Loc.t)]
      in
      Destiny.chain e2 dest__007_; Destiny.chain e1 dest__006_
     ];

    "simple await", 
    [%expr A (A.await e)],
    [%expr
      let dest__003_::[] =
        let v__002_ = A (Loc.dummy () : 't___001_) in
        Destiny.refine dest v__002_
          [(Loc.mk v__002_ 0 : 't___001_ Loc.t)]
      in
      Destiny.chain e dest__003_
    ];
    "simple await with @@", 
    [%expr A (A.await @@ e)],
    [%expr
      let dest__003_::[] =
        let v__002_ = A (Loc.dummy () : 't___001_) in
        Destiny.refine dest v__002_
          [(Loc.mk v__002_ 0 : 't___001_ Loc.t)]
      in
      Destiny.chain e dest__003_
    ];

    "constant polyvar",
    [%expr `A],
    [%expr
      Destiny.fill dest `A
    ];
    "1-polyvar",
    [%expr `A (A.await e)],
    [%expr
      let dest__003_::[] =
        let v__002_ = `A (Loc.dummy () : 't___001_) in
        Destiny.refine dest v__002_
          [(Loc.mk v__002_ 1 : 't___001_ Loc.t)]
      in
      Destiny.chain e dest__003_
    ];
    "tuple-polyvar",
    [%expr `A (A.await e1, 4, A.await e2, 5)],
    [%expr
      let dest__006_::dest__007_::[] =
        let v__005_ =
          ((Loc.dummy () : 't___001_), 4, (Loc.dummy () : 't___003_), 5)
        in
        let v__009_ = `A v__005_ in
        Destiny.refine dest v__009_
          [(Loc.mk v__005_ 0 : 't___001_ Loc.t);
           (Loc.mk v__005_ 2 : 't___003_ Loc.t)]
      in
      Destiny.chain e2 dest__007_; Destiny.chain e1 dest__006_
    ];

    "linear control expr", 
    [%expr A Foo.(f y)],
    [%expr
      let dest__003_::[] =
        let v__002_ = A (Loc.dummy () : 't___001_) in
        Destiny.refine dest v__002_
          [(Loc.mk v__002_ 0 : 't___001_ Loc.t)]
      in
      Destiny.fill dest__003_ (let open Foo in f y)
    ];

    "linear control cons", 
    [%expr A Foo.(B 3)],
    [%expr
      let dest__005_::[] =
        let v__002_ = B 3 in
        let v__004_ = A (Loc.dummy () : 't___003_) in
        Destiny.refine dest v__004_
          [(Loc.mk v__004_ 0 : 't___003_ Loc.t)]
      in
      Destiny.fill dest__005_ (let open Foo in B 3)
    ];

    "linear control await", 
    [%expr A (let x = 3 in A.await (let y = 2 in f x y))],
    [%expr
      let dest__003_::[] =
        let v__002_ = A (Loc.dummy () : 't___001_) in
        Destiny.refine dest v__002_
          [(Loc.mk v__002_ 0 : 't___001_ Loc.t)]
      in
      let x = 3 in
      let y = 2 in AsyncFun.fork (fun () -> f_dps dest__003_ x y)
    ];

    "branching + await", 
    [%expr if b then A.await e else A.await (f x)],
    [%expr
      if b
      then Destiny.chain e dest
      else AsyncFun.fork (fun () -> f_dps dest x)
    ];

    "cons + branching + await", 
    [%expr A (if b then A.await (f x) else A.await e)],
    [%expr
      let dest__003_::[] =
        let v__002_ = A (Loc.dummy () : 't___001_) in
        Destiny.refine dest v__002_
          [(Loc.mk v__002_ 0 : 't___001_ Loc.t)]
      in
      if b
      then AsyncFun.fork (fun () -> f_dps dest__003_ x)
      else Destiny.chain e dest__003_
    ];

    "cons + await + branching", 
    [%expr A (let x = 3 in A.await (if b then f x else e))],
    [%expr
      let dest__003_::[] =
        let v__002_ = A (Loc.dummy () : 't___001_) in
        Destiny.refine dest v__002_
          [(Loc.mk v__002_ 0 : 't___001_ Loc.t)]
      in
      let x = 3 in
      if b
      then AsyncFun.fork (fun () -> f_dps dest__003_ x)
      else Destiny.chain e dest__003_
    ];

    "map async",
    [%expr
      match l with
      | [] -> []
      | x :: xs ->
        f x :: A.await (f aux xs)
    ],
    [%expr
      match l with
      | [] -> Destiny.fill dest []
      | x::xs ->
        let dest__004_::dest__005_::[] =
          let v__003_ =
            (Loc.dummy () : 't___001_) :: (Loc.dummy () : 't___002_)
          in
          Destiny.refine dest v__003_
            [(Loc.mk v__003_ 0 : 't___001_ Loc.t);
             (Loc.mk v__003_ 1 : 't___002_ Loc.t)]
        in
        AsyncFun.fork (fun () -> f_dps dest__005_ aux xs);
        Destiny.fill dest__004_ (f x)
    ]
  ]

(** Test runner. *)

let () =
  Alcotest.run "Ppx_tmaa" [
    "Expression equivalence", expr_tests
  ]
