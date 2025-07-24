let max_parallelism = Domain.recommended_domain_count () (** Maximum number of cores *)
let seed = 123456 (** Seed for random tree gen. *)
let size = 10000 (** Tree size *)
let iterations = 20000. (** Number of iterations *)

let () =
  Fmt.pr "Configuration: %d domains, %d seed, %d tree size, %f iterations@."
    max_parallelism seed size iterations

(** Benchmark*)
open Bench_tree_map_common
open Picos
open Bechamel

let () = Random.init seed

(** Benchmark arguments *)
let tree = gen_tree size

let args = List.init max_parallelism (fun x -> x+1)

let applied_fun x =
  let r = ref 0 in
  for _ = 0 to int_of_float (x *. iterations) do incr r done;
  !r

let bench_tree_sync name f =
  Test.make_indexed ~name ~fmt:"%s %d"
    ~args
    (fun _i -> Staged.stage @@ fun () -> f applied_fun tree)

let bench_tree_async name f = 
  Test.make_indexed_with_resource ~name ~fmt:"%s %d"
    ~args
    ~allocate:(fun i ->
        Moonpool.Ws_pool.create ~num_threads:i ()
      )
    ~free:Moonpool.Runner.shutdown
    Test.uniq
    (fun _i ->
       Staged.stage @@ fun pool ->
       Moonpool.Runner.run_wait_block pool @@ fun () ->
       Computation.await @@ f applied_fun tree)

let indexed_benchs =
  let test_tmc = bench_tree_sync "map_tmc" map_tmc in
  (* let test_seq = bench_tree_async "map_async" map_async in *)
  (* let test_seq_tmca = bench_tree_async "map_async_tmca" map_async_tmca in *)
  let test_par_width = bench_tree_async "map_asyncpar_width" map_asyncpar_width in
  let test_par_width_tmca = bench_tree_async "map_asyncpar_width_tmca" map_asyncpar_width_tmca in
  let test_par_full = bench_tree_async "map_asyncpar_full" map_asyncpar_full in
  let test_par_full_tmca = bench_tree_async "map_asyncpar_full_tmca" map_asyncpar_full_tmca in
  Test.make_grouped ~name:"tree bench" ~fmt:"%s %s"
    [ test_tmc;
      (* test_seq; test_seq_tmca; *)
      test_par_width; test_par_width_tmca; 
      test_par_full; test_par_full_tmca;
    ]

let main () =
  run_benchmark "parscaling.raw.json" indexed_benchs;
  ()

let () = main ()
