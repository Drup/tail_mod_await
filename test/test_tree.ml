(** Test TMAA with a tree example. *)

type tree = N of int * tree * tree | L
[@@deriving eq, show { with_path = false }]

(* A sequential version, for reference. *)

let rec map_sequential f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    N (f v, map_sequential f tl, map_sequential f tr)

(* A manually crafted version in DPS. *)

let rec map_dps_manual d f t =
  let open Ppx_tmaa_runtime in
  match t with
  | L ->
    Destiny.fill d L
  | N (v, t1, t2) ->
    let t : tree = N (f v, Loc.dummy (), Loc.dummy ()) in
    let left : tree Loc.t = Loc.mk t 1 in
    let right : tree Loc.t = Loc.mk t 2 in
    let [d_left; d_right] = Destiny.refine d t [ left ; right ] in
    AsyncFun.fork (fun () -> map_dps_manual d_left f t1);
    AsyncFun.fork (fun () -> map_dps_manual d_right f t2);
    ()

let map_manual f t =
  let open Ppx_tmaa_runtime in
  let d, c = Destiny.mk () in
  AsyncFun.fork (fun () -> map_dps_manual d f t);
  c

(* A fully concurrent automatic version. *)

let%async rec map_tmaa f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    N (f v,
       Computation.await (map_tmaa f tl),
       Computation.await (map_tmaa f tr))

let check tree f_reference f_others =
  let open Alcotest in
  let test = testable pp_tree equal_tree in
  let v_ref = f_reference tree in
  let vs = List.map (fun (s, f) -> s, f tree) f_others in  
  test_case (show_tree tree) `Quick (fun () ->
      List.iter (fun (s, v) ->
          check test s v_ref (Picos.Computation.await v)
        )
        vs
    )

let test_simple_tree () =
  let v = N (4, N (20, L, L), N (12, N (3, L, L), L)) in
  let f x = x + 1 in
  check
    v
    ( map_sequential f )
    [ "Manual DPS", map_manual f;
      "TMAA full", map_tmaa f;
    ]

let () =
  Picos_mux_fifo.run @@ fun () ->
  Alcotest.run "Ppx_tmaa full" [
    "Small tree", [test_simple_tree ()]
  ]
