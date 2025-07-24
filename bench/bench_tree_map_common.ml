open Picos

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

(** Map functions *)

let rec map f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    N (y, map f tl, map f tr)

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

let rec map_async f t = async @@ fun () -> match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_async f tl) in
    let tr' = Computation.await (map_async f tr) in
    N (y, tl', tr')

let rec map_asyncpar_width f t = async @@ fun () -> match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    let task_tl = map_asyncpar_width f tl in
    let task_tr = map_asyncpar_width f tr in
    N (y,
       Computation.await task_tl,
       Computation.await task_tr)

let rec map_asyncpar_full f t = async @@ fun () -> match t with
  | L -> L
  | N (v , tl , tr ) ->
    let task_tl = map_asyncpar_full f tl in
    let task_tr = map_asyncpar_full f tr in
    N (f v,
       Computation.await task_tl,
       Computation.await task_tr)

let%async rec map_async_tmca f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    let tl' = Computation.await (map_async_tmca f tl) in
    N (y, tl', Computation.await (map_async_tmca f tr))

let%async rec map_asyncpar_width_tmca f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let y = f v in
    N (y,
       Computation.await (map_asyncpar_width_tmca f tl),
       Computation.await (map_asyncpar_width_tmca f tr))

let%async rec map_asyncpar_full_tmca f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    N (f v,
       Computation.await (map_asyncpar_full_tmca f tl),
       Computation.await (map_asyncpar_full_tmca f tr))

(** Tree generation *)

let rec cardinal = function
  | L -> 0
  | N(_, l, r) -> cardinal l + cardinal r + 1

let rec height = function
  | L -> 0
  | N(_, l, r) -> 1 + max (height l) (height r)    

let%async rec gen_tree wmax w size =
  let n = Random.int 10 in
  let temp_w = (!w * (100 + n))/100 in 
  if temp_w >= wmax || !size <= 0 then (decr w; L)
  else begin
    decr size;
    incr w;
    let d = Random.float 2. in
    N (d,
       Computation.await (gen_tree wmax w size),
       Computation.await (gen_tree wmax w size))
  end

let gen_tree size =
  let t = Picos_mux_fifo.run @@ fun () ->
    Computation.await @@ gen_tree 40 (ref 0) (ref size)
  in
  t

(** Bechamel boilerplate *)
open Bechamel

let run_benchmark file benchs =
  let instances = [ Toolkit.Instance.monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~stabilize:true ~limit:1000 ~quota:(Time.second 10.) ()
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
