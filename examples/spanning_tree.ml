open Picos
open Graph

module G = Pack.Digraph
module H = Hashtbl.Make(G.V)

type 'a t = Node of 'a * 'a t list | Leaf
[@@deriving show { with_path = false }]

let%async rec spanning_tree_from g h r =
  if H.mem h r then
    Leaf
  else begin
    H.add h r ();
    let label = G.V.label r in
    let nexts = G.succ g r in
    Node (label, Computation.await (spanning_forest_from g h nexts))
  end
    
and spanning_forest_from g h nexts =
  match nexts with
  | [] -> []
  | n :: t ->
    Computation.await (spanning_tree_from g h n) ::
    Computation.await (spanning_forest_from g h t)

let spanning_tree g =
  let r = ref None in
  try
    G.iter_vertex (fun v -> r := Some v; raise Exit) g;
    invalid_arg "spanningtree"
  with Exit ->
  match !r with
  | None -> assert false
  | Some r ->
    let h = H.create (G.nb_vertex g) in
    Computation.await (spanning_tree_from g h r)

let g = G.Rand.gnp ~loops:true ~v:10000 ~prob:0.1 ()

let main () = spanning_tree g

let _t = Picos_mux_fifo.run main
