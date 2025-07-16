let n_domains = 4 (** Degree of parallelism *)
let seed = 123456 (** Seed for random tree gen. *)
let iterations = 20000. (** Number of iterations *)

let () =
  Fmt.pr "Configuration: %d domains, %d seed, %f iterations@."
    n_domains seed iterations

(** Benchmark*)
open Bench_tree_map_common
open Picos
open Bechamel

let () = Random.init seed

(* range from 10³ to 10⁶ elements, 5 points per order. *)
let args = List.init 16 (fun x -> int_of_float (10. ** (float x/.5. +. 3.)))
let trees =
  List.map (fun i -> i, gen_tree i) args
  |> List.to_seq
  |> Hashtbl.of_seq

let applied_fun i x =
  let r = ref 0 in
  for _ = 0 to int_of_float (x *. float i) do incr r done;
  !r

let bench_tree_sync name f = 
  Test.make_indexed ~name ~fmt:"%s %d"
    ~args
    (fun i ->
       Staged.stage @@ fun () ->
       f (applied_fun i) @@ Hashtbl.find trees i)

let bench_tree_async name f = 
  Test.make_indexed ~name ~fmt:"%s %d"
    ~args
    (fun i ->
       Staged.stage @@ fun () ->
       Computation.await @@ f (applied_fun i) @@ Hashtbl.find trees i)

let indexed_benchs =
  let test_tmc = bench_tree_sync "map_tmc" map_tmc in 
  let test_seq = bench_tree_async "map_seq" map_seq in
  let test_par_width = bench_tree_async "map_par_width" map_par_width in
  let test_par_full = bench_tree_async "map_par_full" map_par_full in
  let test_seq_tmca = bench_tree_async "map_seq_tmca" map_seq_tmca in
  let test_par_width_tmca = bench_tree_async "map_par_width_tmca" map_par_width_tmca in
  let test_par_full_tmca = bench_tree_async "map_par_full_tmca" map_par_full_tmca in
  Test.make_grouped ~name:"tree bench" ~fmt:"%s %s"
    [ test_tmc;
      test_seq; test_par_width; test_par_full;
      test_seq_tmca; test_par_width_tmca; test_par_full_tmca;
    ]

let main () =
  run_benchmark "elemscaling.raw.json" indexed_benchs;
  ()

let () =
  let pool = Moonpool.Ws_pool.create ~num_threads:n_domains () in
  Moonpool.Runner.run_wait_block pool main
