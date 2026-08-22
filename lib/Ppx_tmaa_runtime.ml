module P = Picos

module H = Saturn.Htbl
module St = Saturn.Stack

module Loc = struct

  type 'h t = {
    pointer : Obj.t ;
    offset : int ;
  }
  
  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list
  
  let rec length : type a . a list -> int = function
    | [] -> 0
    | _ :: t -> length t + 1

  let mk block offset =
    let pointer = Obj.repr block in
    assert (Obj.is_block pointer);
    {pointer; offset}

  let dummy () = Obj.magic @@ ref ()
      
  (** [set l v] fills the location [l] with [v]. *)
  let set
    : type a . a t -> a -> unit
    = fun loc v ->
      let tag = Obj.tag loc.pointer in
      if tag = Obj.double_array_tag then
        Obj.set_double_field loc.pointer loc.offset (Obj.magic v)
      else
        Obj.set_field loc.pointer loc.offset (Obj.repr v)

  (** [get l] gets the value at location [l]. *)
  let get 
    : type a . a t -> a
    = fun loc ->
      let tag = Obj.tag loc.pointer in
      if tag = Obj.double_array_tag then
        Obj.magic @@ Obj.double_field loc.pointer loc.offset
      else
        Obj.obj @@ Obj.field loc.pointer loc.offset
end


type counter = int Atomic.t

and 'h destiny = {
  hole : 'h Loc.t;
  awaitable : awaitable;
  callback : 'h destiny_callback Atomic.t ;
}

and 'a destiny_callback =
  | Filled of 'a
  | ToFill of 'a Loc.t list

and direct_parent = Root | A of awaitable

and awaitable = {
  id : int ;
  nb_holes : counter ;
  awaitable_children : (awaitable, awaitable) H.t ;
  awaitable_ancestors : (awaitable, awaitable) H.t ;
  direct_parent : direct_parent ; 

  resolve : (unit -> unit) option ;
}

module A = struct
  type t = awaitable
  let equal x y = x.id = y.id
  let hash x = x.id
end


let add_ancestor awaitable ancestor = 
  if awaitable.id <> ancestor.id then 
    let _ = H.try_add awaitable.awaitable_ancestors ancestor ancestor in 
    ()

let add_child awaitable child = 
  if awaitable.id <> child.id then 
    let _ = H.try_add awaitable.awaitable_children child child in 
    ()


let remove_child awaitable child = 
  H.try_remove awaitable.awaitable_children child

let resolve_call r = match r with 
  | None -> () 
  | Some resolve -> resolve ()


let awaitable_update r n =
  assert (n >= -1);
  let oldv = Atomic.fetch_and_add r.nb_holes n in
  let newv = oldv + n in
  assert (newv >= 0);
  ()



let rec delete_and_replace self ancestor_seq children_seq = 
  Seq.iter (fun (_, awaitable) ->
    if awaitable.id <> self.id then begin
      Seq.iter ( fun (_, child) ->
        add_child awaitable child
      ) children_seq ;

      let _ = remove_child awaitable self in 

      delete_and_replace self (H.to_seq awaitable.awaitable_ancestors) children_seq
    end
  ) ancestor_seq

  







let rec awaitable_try_resolve r = 
    try_full r ;
    try_partial r

and decr_awaitable a = 
  awaitable_update a (-1);
  awaitable_try_resolve a

and decr_direct_parent awaitable = match awaitable.direct_parent with
  | Root -> ()
  | A(parent) -> decr_awaitable parent 

and decr_direct_parent_full awaitable = match awaitable.direct_parent with
  | Root -> resolve_call awaitable.resolve
  | A(parent) -> decr_awaitable parent 


and delete self ancestor_seq = 
  Seq.iter (fun (_, awaitable) ->
    if awaitable.id <> self.id then 
      if remove_child awaitable self then begin
        try_full awaitable;
        delete self (H.to_seq awaitable.awaitable_ancestors) 
      end
  ) ancestor_seq

and try_full awaitable = 
  if H.length awaitable.awaitable_children = 0 then begin

    if  Atomic.compare_and_set awaitable.nb_holes (-1) (-2) then begin 
      match awaitable.direct_parent with
        | Root -> resolve_call awaitable.resolve
        | A(_) -> delete awaitable (H.to_seq awaitable.awaitable_ancestors)
    end
    else if Atomic.compare_and_set awaitable.nb_holes 0 (-2)  then begin
      decr_direct_parent_full awaitable ;
      delete awaitable (H.to_seq awaitable.awaitable_ancestors)
    end
  end

and try_partial awaitable = 
  if Atomic.compare_and_set awaitable.nb_holes 0 (-1) then begin
    decr_direct_parent awaitable ;
    delete_and_replace awaitable (H.to_seq awaitable.awaitable_ancestors) (H.to_seq awaitable.awaitable_children)
  end

module Destiny = struct 

  type 'h t = 'h destiny

  (* Polymorphic map for getting subholes in a destiny *)
  
  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list


  let id_number = Atomic.make 0

  let give_id () = Atomic.fetch_and_add id_number 1

  let mk_from awaitable =
    let id = give_id () in 
    let nb_holes = Atomic.make_contended 1 in
    let awaitable_children = H.create ~hashed_type:(module A) () in 
    let awaitable_ancestors = H.create ~hashed_type:(module A) () in 
    let direct_parent = A(awaitable) in
    let resolve = None in 
    { id ; nb_holes ; awaitable_children ; awaitable_ancestors ; direct_parent ; resolve }

  let create_root ~awaitable ~hole =
    { awaitable; callback = Atomic.make (ToFill []); hole }
    
  let create_from ~hole d =
    let awaitable = mk_from d.awaitable in 
    add_ancestor awaitable d.awaitable ;
    add_child d.awaitable awaitable ;
    { awaitable ;
      callback = Atomic.make (ToFill []); hole ;
    }
  
  let rec subdestinies
    : type a. _ t -> a Loc.list -> a list
    = fun d -> function
      | [] -> []
      | hole :: t ->
        let d' = create_from ~hole d in
        d' :: subdestinies d t



  let decr d =
    awaitable_update d.awaitable (-1);
    awaitable_try_resolve d.awaitable
    
  let incr d n =
    awaitable_update d.awaitable n




    

  (* Actual destiny functions. *)

  let set d v =
    let oldv = Atomic.exchange d.callback (Filled v) in
    match oldv with
    | Filled v' -> assert (v == v')
    | ToFill l ->
      List.iter
        (fun h -> Loc.set h (Sys.opaque_identity v))
        (d.hole :: l)

  

  let rec copy ~orig d = 
    Printf.printf "copy\n";
    let c = Atomic.get orig.callback in
    match c with 
    | Filled v -> Loc.set d.hole v
    | ToFill l -> 
      if Atomic.compare_and_set orig.callback c (ToFill (d.hole :: l)) then begin 
        add_child d.awaitable orig.awaitable; 
        add_ancestor orig.awaitable d.awaitable ;
        ()
      end
      else
        copy ~orig d 

  let refine (type a) d v (hs : a Loc.list) : a list =
    set d v;
    match hs with
    | [] ->
      decr d; (*BIG BIG  TODO*)
      []
    | [ hole ] ->
      (* Only one hole. No need to update counter. *)
      [create_from d ~hole]
    | _ -> 
      let locs = Loc.length hs in
      incr d (locs - 1);
      subdestinies d hs
    
  let fill d v =
    let [] = refine d v [] in ()

  let chain (c : 'a P.Computation.t) d = (*TODO*)
    (* TODO: Improve efficiency with scheduler support *)
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ ->
        let v = P.Computation.await c in
        set d v;
        decr d;
        ()
      )

  (* Awaitable initialisation *)


  let mk () = 
    print_string "made a root\n" ; 
    let id = give_id () in
    let nb_holes = Atomic.make_contended 1 in

    let awaitable_children = H.create ~hashed_type:(module A) () in
    let awaitable_ancestors = H.create ~hashed_type:(module A) () in 
    let direct_parent = Root in 


    let computation = P.Computation.create () in
    let value : 'a ref = ref (Loc.dummy ()) in
    let hole : 'a Loc.t = Loc.mk value 0 in
    let resolve () =
      P.Computation.return computation (Loc.get hole)
    in

    let awaitable = { id ; nb_holes ; awaitable_children ; awaitable_ancestors ; direct_parent ; resolve = Some resolve } in
    let d = create_root ~hole ~awaitable in
    d, computation



end 

module Memo = struct

  type ('k, 'a) t = ('k, 'a Destiny.t) H.t

  let init = H.create

  type 'v add_or_get = Added | Found of 'v
  let add_or_get tbl k v =
    if H.try_add tbl k v then
      Added
    else
      Found (H.find_exn tbl k)

  let memo (tbl : _ t) d key f =
    (* Atomically check if [key] has been seen. *)
    match add_or_get tbl key d with
    | Added ->
      (* It wasn't seen, we have the lock. We can execute f *)
      f key
    | Found orig ->
      (* It was seen. We can register the link *)
      Destiny.copy ~orig d;
      (*assert (orig.awaitable == d.awaitable);*)
      (*TODO ASSERT*)
      Destiny.decr d;
      ()

end



module AsyncFun = struct

  let async f =
    let c = P.Computation.create () in
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ -> P.Computation.return c @@ f());
    c
  
  let fork f =
    let c = P.Computation.create () in
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ -> f(); P.Computation.finish c)

end

  

 
