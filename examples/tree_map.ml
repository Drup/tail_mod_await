open Picos

type 'a tree = N of 'a * 'a tree * 'a tree | L
[@@deriving show { with_path = false }]

let%async rec map f t = match t with
  | L -> L
  | N (v , tl , tr ) ->
    N (f v, Computation.await (map f tl), Computation.await (map f tr))

let tree0 = N (4, N (20, L, L), N (12, N (3, L, L), L))

let main () =
  let p = map ((+) 1) tree0 in
  Computation.await p

let () =
  let tree = Picos_mux_fifo.run main in
  Format.printf "Before: %s@." @@ [%show: int tree] tree0;
  Format.printf "After:  %s@." @@ [%show: int tree] tree;
