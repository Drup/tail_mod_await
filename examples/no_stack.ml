open Picos

let len = 50000000 
let l = List.init len (fun i -> i)

(** Various version of map. *)

let rec map_naive f = function
  | [] -> []
  | h :: t -> f h :: map_naive f t

let[@tail_mod_cons] rec map_tmc f l =
  match l with
  | [] -> []
  | x :: xs ->
    let y = f x in
    y :: map_tmc f xs

let%async rec fmap_async f l =
  match l with
  | [] -> []
  | x :: xs ->
    f x :: Computation.await (fmap_async f xs)

let%async rec traverse_async f l =
  match l with
  | [] -> []
  | x :: xs ->
    Computation.await (f x) :: Computation.await (traverse_async f xs)

(** An ID function with progress bar. *)

let counted_id () =
  let r = ref 0 in
  fun x ->
    incr r;
    (* Every 5%, print a +. *)
    if !r mod (len/20) = 0 then
      (print_string "+"; flush_all ()) ;
    x

let start s = 
  print_endline ("Launching "^s);
  Mtime_clock.counter ()
let stop c =
  Gc.full_major ();
  Format.printf "@.Done in %a@.@?"
    Mtime.Span.pp (Mtime_clock.count c)

(** Launch all the functions. *)

let main () =
  let c = start "map_naive" in
  begin try
      ignore @@ map_naive (counted_id ()) l;
      print_newline ();
      assert false
    with Stack_overflow ->
      print_newline ();
      print_endline "Stack_overflow"
  end;
  stop c;
  let c = start "map_tmc" in
  ignore @@ map_tmc (counted_id ()) l;
  stop c;
  let c = start "fmap_async" in
  ignore @@ Computation.await @@ fmap_async (counted_id ()) l;
  stop c;
  let c = start "traverse_async" in
  let f = counted_id () in
  ignore @@ Computation.await @@
  traverse_async (fun e -> Computation.returned @@ f e) l;
  stop c;
  ()

let () = Picos_mux_fifo.run main
