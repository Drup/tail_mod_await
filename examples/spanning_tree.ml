open Picos
open Graph

(** The graph *)

(* A notion of graph *)
module G = Pack.Digraph
module H = Hashtbl.Make(G.V)

(* A node picker, for initialisation. *)
let pick_node g =
  let r = ref None in
  try
    G.iter_vertex (fun v -> r := Some v; raise Exit) g;
    invalid_arg "pick_node"
  with Exit ->
  match !r with
  | None -> assert false
  | Some r -> r

(** Trees *)

type 'a t = Node of 'a * 'a t list
[@@deriving show { with_path = false }]

(** Parallel spanning tree *)

let%async rec spanning_tree_from g seen r =
  let label = G.V.label r in
  let nexts = G.succ g r in
  Node (label, Computation.await (spanning_forest_from g seen nexts))
    
and spanning_forest_from g seen nexts =
  match nexts with
  | [] -> []
  | n :: t ->
    if H.mem seen n then 
      Computation.await (spanning_forest_from g seen t)
    else begin
      H.add seen n n;
      Computation.await (spanning_tree_from g seen n) ::
      Computation.await (spanning_forest_from g seen t)
    end
[@@ocaml.warning "-32"]

let spanning_tree g r =
  let h = H.create (G.nb_vertex g) in
  Computation.await (spanning_tree_from g h r)

(** Sequential spanning tree *)

let rec tree_of_acyclic_graph g r =
  let label = G.V.label r in
  let l = List.map (tree_of_acyclic_graph g) (G.succ g r) in
  Node (label, l)

let kruskal_spanning_tree g r =
  let edges = G.spanningtree g in
  let g' = G.create ~size:(G.nb_vertex g) () in
  List.iter (G.add_edge_e g') edges;
  tree_of_acyclic_graph g' r

(** Set of vertices in a tree *)

module S = Set.Make(Int)

let rec all_elements elems (Node (r, childrens)) =
  let elems = S.add r elems in
  List.fold_left all_elements elems childrens

(** Run everything *)

let start s = 
  print_endline ("Launching "^s);
  Mtime_clock.counter ()
let stop c =
  Gc.full_major ();
  Format.printf "Done in %a@.@?"
    Mtime.Span.pp (Mtime_clock.count c)

let main () =
  (* A random graph *)
  let g0 = G.Rand.graph ~v:1000000 ~e:5000000 () in
  (* An element *)
  let r0 = pick_node g0 in
  let c = start "spanning_tree" in
  let _t_par = spanning_tree g0 r0 in
  stop c;
  let c = start "kruskal" in
  let _t_seq = kruskal_spanning_tree g0 r0 in
  stop c;
  assert (S.equal (all_elements S.empty _t_seq) (all_elements S.empty _t_par));
  ()

let () =
  (* A work-stealing scheduler *)
  let num_threads = 4 in
  let pool = Moonpool.Ws_pool.create ~num_threads () in
  Moonpool.Runner.run_wait_block pool main
