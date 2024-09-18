open Picos

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

let%async rec map f t : 'a tree = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let tr = map f tr in
    N (f v, Computation.await (map f tl), Computation.await tr)
  
let main _ =
  let v = N (4, N (20, L, L), N (12, N (3, L, L), L)) in
  let p = map ((+) 1) v in
  Computation.await p

let () =
  let s = [%show: int tree] @@
    Picos_mux_fifo.run main
  in
  Printf.printf "Map with PPX:\n %s" s
