open Picos

(** Type of binary trees  *)
type 'a tree =
  Node of 'a * 'a tree * 'a tree   (* Node (label, left child, right child) *)
| Leaf                          (* Leaf *)
[@@deriving eq, show { with_path = false }]


(** "Naïve" asynchronous tree map
    [map_seq f t]
    Both recursive calls are pulled out of the constructor so that they are not targeted
    by the transformation
    This is semantically equivalent to (one possible evaluation of)

    {[
    let%async rec map_seq f t = match t with
      | Leaf -> Leaf
      | Node (v , tl , tr ) ->
        Node (f v, Computation.await (map_seq f tl), Computation.await (map_seq f tr))
    ]}

    (with no transformation). The call to [map_seq f tl] has to be fully evaluated
    before the call to [map_seq f tr] is launched; which in turn has to be completed before
    the node is allocated

 *)
let%async rec map_seq f t = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_seq f tl) in
    let tr' = Computation.await (map_seq f tr) in
    Node (y, tl', tr')





(** Tree map with "Sequential" TMCA Only the recursive call to the right child will be optimised,
    garanteeing that there are no added interleavings between each recursive calls (same as before,
    the left recursive call is pulled out of the constructor so that it is not target by TMCA).
    The resulting optimisation is similar to TMC, with the added bonus of removing superfluous
    synchronisations on the optimised side.
    Transformed into:

    {[
    let rec map_seq_tmca_dps d f t : 'a tree = match t with
      | Leaf -> Leaf
      | Node (v , tl , tr ) ->
        let y = f v in
        let tl' = Computation.await (map_seq_tmca f tl) in
        let [d1] = refine d (Node (y, tl', Hole)) in
        fork (fun () -> map_seq_tmca_dps d1 f tr)
    ]}
 *)
let%async rec map_seq_tmca f t = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_seq_tmca f tl) in
    Node (y, tl', Computation.await (map_seq_tmca f tr))


(** Fully parallelised version.
    This introduces maximum parallelism. The Node is allocated before any
    of the arguments are computed, with the parallel recursive calls launched first,
    before the sequential arguements ([f v]).
    Transformed into:

    {[
    let rec map_par_full_tmca_dps d f t : 'a tree = match t with
      | Leaf -> Leaf
      | Node (v , tl , tr ) ->
        let [d1; d2; d3] = refine d (Node (Hole, Hole, Hole)) in
        fork (fun () -> map_par_full_tmca_dps d2 f tl);
        fork (fun () -> map_par_full_tmca_dps d3 f tr);
        fill d1 (f v)
    ]}
 *)
let%async rec map_par_full_tmca f t = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
    Node (f v,
       Computation.await (map_par_full_tmca f tl),
       Computation.await (map_par_full_tmca f tr))


(** This version has unbounded parallelism. It can potentially lead to as many parallel
    tasks running as nodes in the tree, if [f] takes a long time to compute.
    A strategy to mitigate this potential issue would be to take the call to [f] out of
    the node so that it is executed before launching the parallel tasks. This way, the number
    of concurrently running tasks is bounded by the width of the tree:
 *)
let%async rec map_par_width_tmca f t = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
     let y = f v in
     Node (y,
           Computation.await (map_par_width_tmca f tl),
           Computation.await (map_par_width_tmca f tr))
(** This version compiles to:
    {[
    let%async rec map_par_width_tmca f t = match t with
      | Leaf -> Leaf
      | Node (v , tl , tr ) ->
         let y = f v in
        let [d1; d2] = refine d (Node (y, Hole, Hole)) in
        fork (fun () -> map_par_full_tmca_dps d1 f tl);
        fork (fun () -> map_par_full_tmca_dps d2 f tr);
    ]}
 *)


(** * Testing the different optimised versions against the "naïve" one
    on a small example *)


type int_tree = int tree
[@@deriving eq, show {with_path = false}]

let check tree f_reference f_others =
  let open Alcotest in
  let test = testable pp_int_tree equal_int_tree in
  let v_ref = f_reference tree in
  let vs = List.map (fun (s, f) -> s, f tree) f_others in
  test_case (show_int_tree tree) `Quick (fun () ->
      List.iter (fun (s, v) ->
          check test s v_ref (Picos.Computation.await v)
        )
        vs
    )

let test_tree () =
  let v = Node (4, Node (20, Leaf, Leaf), Node (13, Node (12, Leaf, Leaf), Leaf)) in
  let f x = x + 42 in
  check v
    (fun t -> (Computation.await (map_seq f t)))
    [ "Sequential TMCA", map_seq_tmca f ;
      "Parallel TMCA", map_par_full_tmca f;
      "Width-bounded TMCA", map_par_width_tmca f
    ]


let () =
  Picos_mux_fifo.run @@
    fun () ->
    Alcotest.run "Ppx_tmaa full" [
        "Small tree", [test_tree ()]
      ]
