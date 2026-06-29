open Picos

let del_w = 1
let ins_w = 1
let sub_w = 3

type 'a patch =
  | Del of 'a 
  | Ins of 'a 
  | Sub of 'a * 'a
  | None of 'a


type dist_tree = 
  | N of  dist_tree * dist_tree * dist_tree
  (*| S of dist_tree*)
  | L of int


let%async rec wagner_fisher (table[@memo.tbl]) a b i j get compare = 
  let%memo k = (i, j) in 
  match k with
    | (_, 0) -> L(i * del_w )
    | (0, _) -> L(j * ins_w )
    | _ -> 
      if compare (get a (i-1)) (get b (j-1)) then 
        

         Computation.await (wagner_fisher table a b (i-1) (j-1) get compare )
      else
        N(  
          Computation.await (wagner_fisher table a b (i-1) j get compare),
          Computation.await (wagner_fisher table a b i (j-1) get compare),
          Computation.await (wagner_fisher table a b (i-1) (j-1) get compare)        )




let rec min_path t = match t with
  | L(w) -> (w, [])
  | N(t1, t2, t3) ->
    let (w1_raw, p1) = min_path t1 in
    let (w2_raw, p2) = min_path t2 in 
    let (w3_raw, p3) = min_path t3 in
    let (w1, w2, w3) = (w1_raw + del_w, w2_raw + ins_w, w3_raw + sub_w) in 
    
    if w1 < w2 then
      if w1 < w3 then
        ((w1), 1::p1)
      else 
        ((w3), 3::p3)
    else
      if w2 < w3 then 
        ((w2), 2::p2)
      else
        ((w3), 3::p3 ) 
  

let patch t a b i j get compare = 
  let rec prefix a b i j acc = match (i, j) with
    | (0, 0) -> acc
    | (k, 0) -> prefix a b (i-1) j ( (Del(get a (k-1)))::acc)
    | (0, k) -> prefix a b i (j-1) ( (Ins(get b (k-1)))::acc)
    | _ -> acc

  in let rec surfix (acc_l, i, j) p = 
    if i = 0 || j = 0 then (acc_l, i, j) else
    if compare (get a (i-1)) (get b (j-1)) then
      surfix ( (None(get a (i-1)))::acc_l,  (i-1), (j-1)) p
    else
      match p with 
        | 1 -> ( (Del(get a (i-1)))::acc_l, (i-1), j )
        | 2 -> ( (Ins(get b (j-1)))::acc_l, i, (j-1) )
        | 3 -> ( (Sub(get a (i-1), get b (j-1)))::acc_l, (i-1), (j-1) )
        | _ -> ( (None(get a (i-1)))::acc_l, 0, 0 )
  in 
  let (acc, i', j') = List.fold_left surfix ([], i, j) (snd (min_path t)) in 
  prefix a b i' j' acc


let print_patch print patch = match patch with
  | Del(e) -> print_string "Del(";print e; print_string ") "
  | Ins(e) -> print_string "Ins(";print e; print_string ") "
  | None(e) -> print_string "None(";print e; print_string ") "
  | Sub(e, e') -> print_string "Sub(";print e; print_string ", "; print e'; print_string ") "
 


 
    





let main a b i j get compare =
  let table = Ppx_tmaa_runtime.Memo.init () in 
  let tree = Computation.await(wagner_fisher table a b i j get compare  ) in
  patch tree a b i j get compare




let p = Picos_mux_fifo.run @@ fun () -> (main "pomme" "teste" 4 4 String.get (=)  )



let () = List.iter (print_patch print_char) p