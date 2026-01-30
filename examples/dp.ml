(* A collection of dynamic programming problems solved with
   memoisation and automatic parallisation.
*)
open Picos

(** Towers of Hanoi puzzle *)
(* Take tower height as CLI argument.
   Build a tree with only tail calls.
   Memoisation makes it linear.
   Parallelism is bounded by 3.
*)

type rod = Left | Center | Right 
[@@deriving show { with_path = false }]

let other r1 r2 = match r1, r2 with
  | Left, Right | Right, Left -> Center
  | Left, Center | Center, Left -> Right
  | Right, Center | Center, Right -> Left
  | Left, Left | Right, Right | Center, Center -> assert false

type solution =
  | Move of rod * rod
  | Concat of solution * solution * solution
[@@deriving show { with_path = false }]

let%async rec hanoi (tbl[@memo.tbl]) n r1 r2 =
  let%memo _ = (n, r1, r2) in
  if n = 1 then Move (r1, r2)
  else begin
    let r3 = other r1 r2 in
    Concat (
      Computation.await @@ hanoi tbl (n-1) r1 r3,
      Move (r1, r2),
      Computation.await @@ hanoi tbl (n-1) r3 r2
    )
  end

(** Matrix chain multiplication *)
(* Takes length chain as CLI argument.
   No tail-call.
   Memoisation makes it O(n³).
   Following implementation is embarrassingly parallel.
*)

type matrix = int * int
[@@deriving show { with_path = false }]
type chain = matrix array
[@@deriving show { with_path = false }]
type mult = Matrix of matrix | Mult of mult * mult
[@@deriving show { with_path = false }]

module K = struct
  type t = K of int * int
  let hash = Hashtbl.hash
  (* let equal (K (i1, i2)) (K (j1, j2)) = i1 = j1 && i2 = j2 *)
  let equal = (=)
end

(* Cost of a i k j division of the input chain. *) 
let cost input i k j =
  (* 0 ≤ i < k < j ≤ n - 1 *)
  fst input.(i) * snd input.(k) * snd input.(j)

let gen_chain n : chain =
  let bound = 100 in
  let rec aux last n =
    if n = 1 then [(last, Random.int bound)]
    else
      let next = Random.int bound in
      (last, next) :: aux next (n-1)
  in
  Array.of_list @@ aux (Random.int bound) n

let%async rec map_par f l =
  match l with
  | [] -> []
  | h :: l ->
    let trest = map_par f l in
    Computation.await (f h) :: Computation.await trest

let%async rec matrix_chain_order (tbl[@memo.tbl]) (input : chain) start stop =
  Format.eprintf "Computing %i:%i@." start stop;
  let%memo _  = K.K (start, stop) in
  if start + 1 = stop then
    let v = 0, Matrix input.(start) in
    v
  else
    let cases = CCList.((start+1) -- (stop-1)) in
    let cases =
      Computation.await @@
      map_par (matrix_chain_order_case tbl input start stop) cases
    in
    let red (c1, m1) (c2, m2) = if c1 <= c2 then (c1, m1) else (c2, m2) in
    CCList.reduce_exn red cases

and matrix_chain_order_case tbl input start stop k =
  let tl = matrix_chain_order tbl input start k
  and tr = matrix_chain_order tbl input k stop in
  let cl, ml = Computation.await tl and cr, mr = Computation.await tr in
  let cost =  cl + cost input start k stop + cr in
  cost, Mult (ml, mr)

(** Full setup. *)
              
let pp_size x =
  Format.eprintf
    "Total size : %.3FMb@."
    (float (Obj.reachable_words (Obj.repr x)) /. 1024. /. 1024.)

let () = Random.self_init ()

let main () =
  let () = match Sys.argv.(1) with
    | "hanoi" ->
      let tbl = Ppx_tmaa_runtime.Memo.init () in
      pp_size
      @@ Computation.await
      @@ hanoi tbl (int_of_string Sys.argv.(2)) Left Right
    | "mult" ->
      let n = int_of_string Sys.argv.(2) in
      let chain = gen_chain n in
      Format.eprintf "Mult : %a@." pp_chain chain;
      let tbl = Ppx_tmaa_runtime.Memo.init ~hashed_type:(module K) () in
      (* pp_size *)
      let v = Computation.await @@ matrix_chain_order tbl chain 0 (n-1) in 
      Format.eprintf "Sol: %a@." pp_mult @@ snd v;
      pp_size v      
    | s -> invalid_arg s
  in
  ()

let () =
  Picos_mux_multififo.run_on ~n_domains:4 main
