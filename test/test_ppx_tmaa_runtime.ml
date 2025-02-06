open Ppx_tmaa_runtime

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

let rec map_dps d f t = match t with
  | L ->
    Destiny.fill d L
  | N (v, t1, t2) ->
    let t : int tree = N (f v, Loc.dummy, Loc.dummy) in
    let left : ('a tree) Loc.t = Loc.mk t 1 in
    let right : ('a tree) Loc.t = Loc.mk t 2 in
    let [d_left; d_right] = Destiny.refine d t [ left ; right ] in

    AsyncFun.fork (fun () -> map_dps d_left f t1);
    AsyncFun.fork (fun () -> map_dps d_right f t2);
    ()

let map f t =
  let d, c = Destiny.mk () in
  AsyncFun.fork (fun () -> map_dps d f t);
  c
  
let main _ =
  let v = N (4, N (20, L, L), N (12, N (3, L, L), L)) in
  let p = map ((+) 1) v in
  Picos.Computation.await p


let () =
  let s = [%show: int tree] @@
    Picos_mux_fifo.run main
  in
  Printf.printf "Map with Handwritten:\n %s" s
