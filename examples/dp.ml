(* A collection of dynamic programming problems solved with
   memoisation and automatic parallisation.
*)
open Picos

(* Towers of Hanoi puzzle *)

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

let pp_size x =
  Format.eprintf
    "Total size : %.3FMb@."
    (float (Obj.reachable_words (Obj.repr x)) /. 1024. /. 1024.)


let main () =
  let tbl = Ppx_tmaa_runtime.Memo.init () in
  let () = match Sys.argv.(1) with
    | "hanoi" ->
      pp_size
      @@ Computation.await
      @@ hanoi tbl (int_of_string Sys.argv.(2)) Left Right
    | s -> invalid_arg s
  in
  ()

let () =
  Picos_mux_multififo.run_on ~n_domains:4 main
