open Picos

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

let[@tail_mod_cons] rec map_tmc f t : 'a tree = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    let tl' = map_tmc f tl in
    N (y, tl', map_tmc f tr)

let async e =
  let p = Computation.create () in
  let fiber = Fiber.create ~forbid:false p in
  Fiber.spawn fiber (fun _ -> Computation.return p @@ e ());
  p

let rec map_seq f t = async @@ fun () -> match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_seq f tl) in
    let tr' = Computation.await (map_seq f tr) in
    N (y, tl', tr')

(* let rec map_par_width f t = async @@ fun () -> match t with *)
(*   | L -> L *)
(*   | N (v , tl , tr ) -> *)
(*     let y = f v in *)
(*     let task_tl = map_par_width f tl in *)
(*     let task_tr = map_par_width f tr in *)
(*     N (y, *)
(*        Computation.await task_tl, *)
(*        Computation.await task_tr) *)

(* let rec map_par_full f t = async @@ fun () -> match t with *)
(*   | L -> L *)
(*   | N (v , tl , tr ) -> *)
(*     let task_tl = map_par_full f tl in *)
(*     let task_tr = map_par_full f tr in *)
(*     N (f v, *)
(*        Computation.await task_tl, *)
(*        Computation.await task_tr) *)

let%async rec map_seq_tmca f t : 'a tree = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_seq_tmca f tl) in
    N (y, tl', Computation.await (map_seq_tmca f tr))

(* let%async rec map_par_width_tmca f t : 'a tree = match t with *)
(*   | L -> L *)
(*   | N (v , tl , tr ) -> *)
(*     let y = f v in *)
(*     N (y, *)
(*        Computation.await (map_par_width_tmca f tl), *)
(*        Computation.await (map_par_width_tmca f tr)) *)

let%async rec map_par_full_tmca f t : 'a tree = match t with
  | L -> L
  | N (v , tl , tr ) ->
    N (f v,
       Computation.await (map_par_full_tmca f tl),
       Computation.await (map_par_full_tmca f tr))


let rec gen_tree size =
  if size = 0 then L
  else
    let left = Random.int size in
    let right = size - 1 - left in
    N (Random.float 0.01, gen_tree left, gen_tree right)

(** Benchmark*)
open Bechamel

let () = Random.init 123456

let args = [ (* 100; *) (* 1000; *) (* 10_000; *)100_000; (* 1_000_000; *) (* 5_000_000 *) ]
let trees =
  List.map (fun i -> i, gen_tree i) args
  |> List.to_seq
  |> Hashtbl.of_seq

let bench_tree_sync name f = 
  Test.make_indexed ~name ~fmt:"%s %d"
    ~args
    (fun i -> Staged.stage @@ fun () -> f @@ Hashtbl.find trees i)

let bench_tree_async name f = 
  Test.make_indexed ~name ~fmt:"%s %d"
    ~args
    (fun i -> Staged.stage @@ fun () -> Computation.await @@ f @@ Hashtbl.find trees i)

let indexed_benchs =
  let f x = Unix.sleepf (x/.20.) in
  (* let f x = x *. 12. in *)
  let test_tmc = bench_tree_sync "map_tmc" @@ map_tmc f in 
  let test_seq = bench_tree_async "map_seq" @@ map_seq f in
  (* let test_par_width = bench_tree_async "map_par_width" @@ map_par_width f in *)
  (* let test_par_full = bench_tree_async "map_par_full" @@ map_par_full f in *)
  let test_seq_tmca = bench_tree_async "map_seq_tmca" @@ map_seq_tmca f in
  (* let test_par_width_tmca = bench_tree_async "map_par_width_tmca" @@ map_par_width_tmca f in *)
  let test_par_full_tmca = bench_tree_async "map_par_tmca" @@ map_par_full_tmca f in
  Test.make_grouped ~name:"tree bench" ~fmt:"%s %s"
    [ test_tmc;
      test_seq; (* test_par_width; test_par_full; *)
      test_seq_tmca; (* test_par_width_tmca; *) test_par_full_tmca;
    ]


let benchmark () =
  let instances = [ Toolkit.Instance.monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~limit:100 ~stabilize:false
      ~quota:(Time.second 10.) () in
  let raw_results =
    Benchmark.all cfg instances indexed_benchs
  in
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]
  in  
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)


let main () =
  let open Bechamel_js in
  let nothing _ = Ok () in
  let dst = channel "report.json" in
  let results = benchmark () in
  let results =
    emit ~dst nothing ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Toolkit.Instance.monotonic_clock)
      results
  in
  match results with Ok () -> () | Error (`Msg err) -> invalid_arg err

(* let main () = *)
(*   let results, _ = benchmark () in *)
(*   match *)
(*     Hashtbl.fold *)
(*       (fun _ v a -> Hashtbl.fold (fun k v a -> (k, v) :: a) v [] :: a) *)
(*       results [] *)
(*   with *)
(*   | [ results ] -> *)
(*       let print (k, ols) = Fmt.pr "%s: %a\n%!" k Analyze.OLS.pp ols in *)
(*       List.iter print results *)
(*   | _ -> assert false *)

(* let main () = *)
(*   let open Notty_unix in *)
(*   let img (window, results) = *)
(*     Bechamel_notty.Multiple.image_of_ols_results ~rect:window *)
(*       ~predictor:Measure.run results *)
(*   in  *)
(*   Bechamel_notty.Unit.add *)
(*     Toolkit.Instance.monotonic_clock *)
(*     (Measure.unit Toolkit.Instance.monotonic_clock); *)
(*   let window = *)
(*     match winsize Unix.stdout with *)
(*     | Some (w, h) -> { Bechamel_notty.w; h } *)
(*     | None -> { Bechamel_notty.w= 80; h= 1; } in *)
(*   let results, _ = benchmark () in *)
(*   img (window, results) |> eol |> output_image *)

let () = Picos_mux_random.run_on ~n_domains:4 main
