open Picos

let del_w = 1
let ins_w = 1
let subst_w = 3

module type COMPARABLE = sig
  type t
  val compare : t -> t -> bool
  val del : t -> int
  val ins : t -> int 
  val subst : t -> t -> int
end



module C : COMPARABLE with type t = char = struct
  type t = char 
  let compare = (*let () = Thread.delay 0.01 in*) (=)
  let del (_:t) = del_w
  let ins (_:t) = ins_w
  let subst (_:t) (_:t) = subst_w

end

type 'a patch =
  | Del of 'a 
  | Ins of 'a 
  | Subst of 'a * 'a
  | Skip 


type dist_tree = 
  | L of leaf
  | S of dist_tree
  | N of node
and leaf = { key : (int * int) ; base : int }
and node = { key : (int * int) ; del : dist_tree ; del_w : int ; ins : dist_tree ; ins_w : int ; subst : dist_tree ; subst_w : int }


let rec root_val i f acc = match i with
  | 0 -> acc
  | _ -> root_val (i-1) f (acc + f i)

let%async rec wagner_fisher m (table[@memo.tbl]) a b i j = 

  let%memo k = (i, j) in 
  let (module M : COMPARABLE with type t = char) = m in
  match k with
    | (_, 0) ->  L { key = k ; base = root_val j (fun k -> M.del (String.get b (k-1))) 0 } 
    | (0, _) ->  L { key = k ; base = root_val i (fun k -> M.del (String.get a (k-1))) 0 }
    | _ -> 
      if M.compare (String.get a (i-1)) (String.get b (j-1)) then 
        S(Computation.await (wagner_fisher m table a b (i-1) (j-1) ))
      else

        N { key = (i, j) ;
      del = Computation.await (wagner_fisher m table a b (i-1) j ) ;
    del_w = M.del (String.get a (i-1)) ; 
      ins= Computation.await (wagner_fisher m table a b i (j-1) )  ;
    ins_w = M.ins (String.get b (j-1)) ; 
      subst = Computation.await (wagner_fisher m table a b (i-1) (j-1) ) ;
    subst_w = M.subst (String.get a (i-1)) (String.get b (j-1))
  }




let min_t a b c = min a (min b c)

let min_t_par (a, w_a) (b, w_b) (c, w_c) =
  min_t (Computation.await a + w_a) (Computation.await b + w_b) (Computation.await c + w_c) 


let rec min_seq tree = match tree with
  | L { base ; _ } -> base 
  | S(t) -> min_seq t
  | N { del ; ins ; subst ; del_w ; ins_w ; subst_w ; _ } -> 
    let (w_del, w_ins, w_subst) = (min_seq del + del_w, min_seq ins + ins_w, min_seq subst + subst_w) in 
    let v = min_t w_del w_ins w_subst in v

let%async rec min_par tree = match tree with 
  | L { base ; _} -> base
  | S(t) -> Computation.await (min_par t)
  | N {del ; del_w ; ins ; ins_w ;  subst ; subst_w ; _} -> 
    let (w_del, w_ins, w_subst) = (min_par del , min_par ins , min_par subst ) in 
    min_t_par (w_del, del_w) (w_ins, ins_w) (w_subst, subst_w)

let rec min_seq_memo table tree = match tree with 
  | L { base ; _} -> base
  | S (t) -> min_seq_memo table t
  | N { key = (i, j) ; del ; ins ; subst ; del_w ; ins_w ; subst_w } -> 
    if table.(i-1).(j-1) <> -1 then
      table.(i-1).(j-1) 
    else 
      let (w_del, w_ins, w_subst) = (min_seq_memo table del + del_w, min_seq_memo table ins + ins_w, min_seq_memo table subst + subst_w) in 
      let v = min_t w_del w_ins w_subst in 
      table.(i-1).(j-1) <- v;
      v


let rec surfix a i = match i with 
    | 0 -> [] 
    | k -> Ins((String.get a (i-1))) :: (surfix a (k-1) )

let rec min_patch tree a b min = match tree with
  |L { key = (i, j) ; _ } -> 
    if i < j then 
      surfix b j
    else 
      surfix a i 
  |S(t) -> Skip::(min_patch t a b min)
  |N { key = (i, j) ; del ; del_w ; ins ; ins_w ; subst ; subst_w } -> 
    let (w_del, w_ins, w_subst) = (min del + del_w, min ins + ins_w, min subst + subst_w) in 
    if w_del < w_ins then 
      if w_del < w_subst then
        Del(String.get a (i-1))::(min_patch del a b min)
      else 
        Subst(String.get a (i-1), String.get b (j-1))::(min_patch subst a b min)
    else
      if w_ins < w_subst then
        Ins(String.get b (j-1))::(min_patch ins a b min)
      else 
        Subst(String.get a (i-1), String.get b (j-1))::(min_patch subst a b min)


let%async rec min_patch_async tree a b min = match tree with
  |L { key = (i, j) ; _ } -> 
    if i < j then 
      surfix b j
    else 
      surfix a i 
  |S(t) -> Skip::(Computation.await (min_patch_async t a b min))
  |N { key = (i, j) ; del ; del_w ; ins ; ins_w ; subst ; subst_w } -> 
    let (w_del, w_ins, w_subst) = (Computation.await (min del) + del_w, Computation.await (min ins) + ins_w, Computation.await (min subst) + subst_w) in 
    if w_del < w_ins then 
      if w_del < w_subst then
        Del(String.get a (i-1))::(Computation.await (min_patch_async del a b min))
      else 
        Subst(String.get a (i-1), String.get b (j-1))::(Computation.await (min_patch_async subst a b min))
    else
      if w_ins < w_subst then
        Ins(String.get b (j-1))::(Computation.await (min_patch_async ins a b min))
      else 
        Subst(String.get a (i-1), String.get b (j-1))::(Computation.await (min_patch_async subst a b min))



let print_patch print patch = match patch with
  | Del(e) -> print_string "Del(";print e; print_string ") "
  | Ins(e) -> print_string "Ins(";print e; print_string ") "
  | Skip -> print_string "Skip "
  | Subst(e, e') -> print_string "Sub(";print e; print_string ", "; print e'; print_string ") "
 


let start s = 
  print_endline ("Launching "^s);
  Mtime_clock.counter ()
let stop c =
  Gc.full_major ();
  Format.printf "@.Done in %a@.@?"
    Mtime.Span.pp (Mtime_clock.count c)

    



let main a b i j =
  let table = Ppx_tmaa_runtime.Memo.init () in 
  let tree = Computation.await(wagner_fisher (module (C : COMPARABLE with type t = char)) table a b i j  ) in

  let table2 = Array.init_matrix (i) (j) (fun _ _ -> -1) in 

  let () = print_string "min seq\n" in
  let c1 = start "min_seq" in
  let patch = min_patch tree a b min_seq in
  stop c1;
  let c2 = start "min_par" in
  let patch2 = Computation.await ( min_patch_async tree a b min_par )in
  stop c2;
  let c3 = start "min_seq_memo"in
  let patch3= min_patch tree a b (min_seq_memo table2) in
  stop c3;
  List.iter (print_patch print_char) patch ; 
  print_endline "";
  List.iter (print_patch print_char) patch2 ; 
  print_endline "";
  List.iter (print_patch print_char) patch3

let s1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non risus. Suspendisse lectus tortor, dignissim sit amet, adipiscing nec, ultricies sed, dolor. Cras elementum ultrices diam. Maecenas ligula massa, varius a, semper congue, euismod non, mi. Proin porttitor, orci nec nonummy molestie, enim est eleifend mi, non fermentum diam nisl sit amet erat. Duis semper. Duis arcu massa, scelerisque vitae, consequat in, pretium a, enim. Pellentesque congue. Ut in risus volutpat libero pharetra tempor. Cras vestibulum bibendum augue. Praesent egestas leo in pede. Praesent blandit odio eu enim. Pellentesque sed dui ut augue blandit sodales. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Aliquam nibh. Mauris ac mauris sed pede pellentesque fermentum. Maecenas adipiscing ante non diam sodales hendrerit." 
let s2 = "lndormi bonjour dolor bsih amet, cosnectpetur adipiscing velit. Seed non risus. Suspendisse lectus tortor, wdiginssim sit amet, adipiscing nmzc, ultricies csed, dolor. Car elementum ulptrices diam. Maecenas ligulba massa, vArius a, semper congue, euismod non, mi. ProBn porttitor, Grci nec nnoummy molestie, enmmi est ffeleifend jmi, no fermentum dam nsl sit aamet ry. DuiJ semper. DDuis aarcu masU, sceleirsique vitae, cconseuat ni, pretium a, neim. Pmlllentesque congue. U in risus volhutpat libero dpharetra tempor. Cras vestibulum bibendum aygue. Praesent egestas leo iA ped. Praesent blanwdit dio Su enim. Penlentesque vsed uii ust augue bFandit sodales. Vestibuulm ante ipsum primiis iin faucibsu orcii luctus et ultrices posuere cubilia Curae; Aliquam nibh. aMuuris ac mauuris ssemd pdee pkellenteysque fermentm. Maecenats adipiscing annte non diam soaQlers hendrerit."

let len = 9
(*let () = Picos_mux_fifo.run @@ fun () -> (main s1 s2 847 859  ) *)


let pool = Moonpool.Fifo_pool.create ~num_threads:12 ()
let () = Moonpool.Runner.run_wait_block pool @@ fun () -> (print_string "test" ; main s1 s2 len len  )

