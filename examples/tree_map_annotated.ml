open Picos

(** Type of binary trees  *)
type 'a tree =
  Node of 'a * 'a tree * 'a tree   (* Node (label, left child, right child) *)
| Leaf                          (* Leaf *)
[@@deriving show { with_path = false }]


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

    (with no transformation). The evaluation of [map_seq f tl] has to be fully evaluated
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





(** Tree map with "Sequential" TMCA
    Only the recursive call to the right child will be optimised
    (same as before, the left recursive call is pulled out of the constructor
    so that it is not target by TMCA).
    The resulting optimisation is similar to TMC, with the added bonus of removing
    superluous synchronisations on the optimised side.
    Transformed into

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
let%async rec map_seq_tmca f t : 'a tree = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_seq_tmca f tl) in
    Node (y, tl', Computation.await (map_seq_tmca f tr))


(** Fully parallelised version.
    This introduces maximum parallelism. The Node is allocated before any
    of the arguments are computed, with the parallel recursive calls launched first,
    before the sequential arguements ([f v]).
    Transformed into

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
let%async rec map_par_full_tmca f t : 'a tree = match t with
  | Leaf -> Leaf
  | Node (v , tl , tr ) ->
    Node (f v,
       Computation.await (map_par_full_tmca f tl),
       Computation.await (map_par_full_tmca f tr))
