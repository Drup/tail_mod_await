open Ppx_tmaa_runtime
open Eio

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

let rec map_dps ~sw d f t = match t with
  | L ->
    Destiny.fill d L
  | N (v, t1, t2) ->
    let left : ('a tree) Destiny.hole = Field (1, Obj) in
    let right : ('a tree) Destiny.hole = Field (2, Obj) in
    let t : int tree = N (f v, Destiny.hole left, Destiny.hole right) in
    let [d_left; d_right] = Destiny.refine d t [ left ; right ] in

    Fiber.fork ~sw (fun () -> map_dps ~sw d_left f t1);
    map_dps ~sw d_right f t2
let map f t =
  Switch.run @@ fun sw ->
  let p, resolver = Promise.create () in
  let d = Destiny.create (Promise.resolve resolver) in
  Fiber.fork ~sw (fun () -> map_dps ~sw d f t);
  p
  
let main _ =
  let v = N (4, N (20, L, L), N (12, N (3, L, L), L)) in
  let p = map ((+) 1) v in
  Promise.await p


let () =
  let s = [%show: int tree] @@ Eio_main.run main in
  print_endline s
