open Picos

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

(** Map functions *)

let[@tail_mod_cons] rec map_tmc f t = match t with
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

let rec map_par_width f t = async @@ fun () -> match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    let task_tl = map_par_width f tl in
    let task_tr = map_par_width f tr in
    N (y,
       Computation.await task_tl,
       Computation.await task_tr)

let rec map_par_full f t = async @@ fun () -> match t with
  | L -> L
  | N (v , tl , tr ) ->
    let task_tl = map_par_full f tl in
    let task_tr = map_par_full f tr in
    N (f v,
       Computation.await task_tl,
       Computation.await task_tr)

let%async rec map_seq_tmca f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_seq_tmca f tl) in
    N (y, tl', Computation.await (map_seq_tmca f tr))

let%async rec map_par_width_tmca f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    N (y,
       Computation.await (map_par_width_tmca f tl),
       Computation.await (map_par_width_tmca f tr))

let%async rec map_par_full_tmca f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    N (f v,
       Computation.await (map_par_full_tmca f tl),
       Computation.await (map_par_full_tmca f tr))

(** Tree generation *)

let rec gen_tree size =
  if size = 0 then L
  else
    let left = Random.int size in
    let right = size - 1 - left in
    N (Random.float 2., gen_tree left, gen_tree right)

(** Bechamel boilerplate *)
open Bechamel

let run_benchmark file benchs =
  let instances = [ Toolkit.Instance.monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~stabilize:false ~limit:1000 ~quota:(Time.second 10.) ()
  in
  let raw_results =
    Benchmark.all cfg instances benchs
  in
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]
  in  
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  let dst = Bechamel_js.channel file in
  let nothing _ = Ok () in
  let results =
    Bechamel_js.emit ~dst nothing ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Toolkit.Instance.monotonic_clock)
      (results, raw_results)
  in
  match results with Ok () -> () | Error (`Msg err) -> invalid_arg err
