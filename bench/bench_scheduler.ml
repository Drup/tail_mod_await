let max_parallelism = Domain.recommended_domain_count () (** Maximum number of cores *)
let seed = 123456 (** Seed for random tree gen. *)
let size = 5000 (** Tree size *)
let iterations = 10000. (** Number of iterations *)

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

let bench_picos_random name f =
  Test.make_indexed ~name ~fmt:"picos random %s %d"
    ~args
    (fun i ->
       Staged.stage @@ fun () ->
       Picos_mux_random.run_on ~n_domains:i  @@ fun () ->
       Computation.await @@ f applied_fun tree)

let bench_picos_multififo name f =
  Test.make_indexed ~name ~fmt:"picos multififo %s %d"
    ~args
    (fun i ->
       Staged.stage @@ fun () ->
       Picos_mux_multififo.run_on ~n_domains:i  @@ fun () ->
       Computation.await @@ f applied_fun tree)

let bench_moonpool_ws name f =
  Test.make_indexed_with_resource ~name ~fmt:"moonpool ws %s %d"
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

let bench_moonpool_fifo name f =
  Test.make_indexed_with_resource ~name ~fmt:"moonpool fifo %s %d"
    ~args
    ~allocate:(fun i ->
        Moonpool.Fifo_pool.create ~num_threads:i ()
      )
    ~free:Moonpool.Runner.shutdown
    Test.uniq
    (fun _i ->
       Staged.stage @@ fun pool ->
       Moonpool.Runner.run_wait_block pool @@ fun () ->
       Computation.await @@ f applied_fun tree)

let indexed_benchs =
  Test.make_grouped ~name:"tree bench" ~fmt:"%s %s"
    [ bench_tree_sync "map_tmc" map_tmc;
      bench_picos_multififo "map_asyncpar_tmca" map_asyncpar_full_tmca;
      bench_picos_random "map_asyncpar_tmca" map_asyncpar_full_tmca;
      bench_moonpool_ws "map_asyncpar_tmca" map_asyncpar_full_tmca;
      bench_moonpool_fifo "map_asyncpar_tmca" map_asyncpar_full_tmca;
    ]

let main () =
  run_benchmark "scheduler.raw.json" indexed_benchs;
  ()

let () = main ()
