open Picos

(** Type of binary trees  *)
type 'a tree =
  Node of 'a * 'a tree * 'a tree   (* Node (label, left child, right child) *)
| Leaf                          (* Leaf *)
[@@deriving show { with_path = false }]


(** "Naïve" asynchronous tree map

    Both recursive calls are pulled out of the constructor so that they are not targeted
    by the transformation.
 *)
let%async rec map_seq f t = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_seq f tl) in
    let tr' = Computation.await (map_seq f tr) in
    Node (y, tl', tr')
(** This is semantically equivalent to (one possible evaluation of)

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




(** Tree map with "Sequential" TMCA.

    Only the recursive call to the right child will be optimised, garanteeing that there are no
    added interleavings between each recursive calls (same as before, the left recursive call is
    pulled out of the constructor so that it is not target by TMCA).

    The resulting optimisation is similar to TMC, with the added bonus of removing superfluous
    synchronisations on the optimised side.
 *)
let%async rec map_seq_tmca f t = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_seq_tmca f tl) in
    Node (y, tl', Computation.await (map_seq_tmca f tr))
(** This version would be transformed into:

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



(** Fully parallelised version.

    This introduces maximum parallelism. The Node is allocated before any
    of the arguments are computed, with the parallel recursive calls launched first,
    before the sequential arguements ([f v]).
 *)
let%async rec map_par_full_tmca f t = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
    Node (f v,
       Computation.await (map_par_full_tmca f tl),
       Computation.await (map_par_full_tmca f tr))
(**
    This is transformed into:

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


(** [map_par_full_tmca] has unbounded parallelism. It can potentially lead to as many parallel
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


(** Testing the different optimised versions against the "naïve" one
    on a small example *)


let test_tree () =
  let v = Node (4, Node (20, Leaf, Leaf), Node (13, Node (12, Leaf, Leaf), Leaf)) in
  let f x = x + 42 in
  Format.printf "Before: %s@." @@ [%show: int tree] v;
  Format.printf "Naïve: %s@." @@ [%show: int tree] (Computation.await (map_seq f v));
  Format.printf "TMCA Sequential: %s@." @@ [%show: int tree] (Computation.await (map_seq_tmca f v));
  Format.printf "TMCA Parallel: %s@." @@ [%show: int tree] (Computation.await (map_par_full_tmca f v));
  Format.printf "TMCA Width-bounded: %s@." @@ [%show: int tree] (Computation.await (map_par_width_tmca f v))


let () =
  Picos_mux_fifo.run @@ test_tree
