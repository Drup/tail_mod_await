open Picos

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

let%async rec map f t : 'a tree = match t with
  | L -> L
  | N (v , tl , tr ) ->
    let tr = map f tr in
    N (f v, Computation.await (map f tl), Computation.await tr)

let tree0 = N (4, N (20, L, L), N (12, N (3, L, L), L))

let main () =
  let p = map ((+) 1) tree0 in
  Computation.await p

let () =
  let tree = Picos_mux_fifo.run main in
  Format.printf "Before: %s@." @@ [%show: int tree] tree0;
  Format.printf "After:  %s@." @@ [%show: int tree] tree;
