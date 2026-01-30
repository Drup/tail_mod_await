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


type counter = {
  nb : int Atomic.t ;
  parent : counter_parent ;
  notify : root St.t ;
}

and counter_parent =
  | Counter of counter
  | Root of root

and 'h destiny = {
  hole : 'h Loc.t;
  last_counter : counter ;
  root : root;
  callback : 'h destiny_callback Atomic.t ;
}

and 'a destiny_callback =
  | Filled of 'a
  | ToFill of 'a Loc.t list

and root = {
  direct_holes : int Atomic.t ;
  indirect_counters : int Atomic.t ;
  resolve : unit -> unit ;
}

let root_try_resolve r =
  if Atomic.get r.direct_holes = 0 then
    if Atomic.compare_and_set r.indirect_counters 0 (-1) then
      r.resolve ()

let root_update_direct r n =
  let _ = Atomic.fetch_and_add r.direct_holes n in
  (* let newv = oldv + n in *)
  (* assert (newv >= 0); *)
  root_try_resolve r

let root_incr_indirect r =
  Atomic.incr r.indirect_counters
let root_decr_indirect r = 
  Atomic.decr r.indirect_counters;
  root_try_resolve r

module Counter = struct
  
  let counter parent n =
    let nb = Atomic.make n in
    let notify = St.create () in
    { nb ; parent ; notify }

  let checkif0 c n =
    if n = 0 then begin
      let l = St.pop_all c.notify in
      List.iter root_decr_indirect l
    end

  let register c r =
    if Atomic.get c.nb = 0 then
      ()
    else begin
      root_incr_indirect r;
      St.push c.notify r;
      checkif0 c (Atomic.get c.nb)
    end

  let rec update c n =
    assert (n >= -1);
    let oldv = Atomic.fetch_and_add c.nb n in
    let newv = oldv + n in
    assert (newv >= 0);
    checkif0 c newv;
    begin match c.parent with
      | Root r -> root_update_direct r n
      | Counter c -> update c n
    end

end

module Destiny = struct

  type 'h t = 'h destiny

  (* Polymorphic map for getting subholes in a destiny *)
  
  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  let create ~root ~hole ~counter =
    { root; callback = Atomic.make (ToFill []); hole ; last_counter = counter }
  let create_from ?counter ~hole d =
    let counter = Option.value counter ~default:d.last_counter in
    create ~root:d.root ~hole ~counter
  
  let rec subdestinies
    : type a. _ t -> a Loc.list -> a list
    = fun d -> function
      | [] -> []
      | hole :: t ->
        let counter = Counter.counter (Counter d.last_counter) 1 in
        let d' = create_from ~hole ~counter d in
        d' :: subdestinies d t

  let decr d =
    Counter.update d.last_counter (-1)
  let incr d n =
    Counter.update d.last_counter n

  (* Actual destiny functions. *)

  let set d v =
    let oldv = Atomic.exchange d.callback (Filled v) in
    match oldv with
    | Filled v' -> assert (v == v')
    | ToFill l ->
      List.iter
        (fun h -> Loc.set h (Sys.opaque_identity v))
        (d.hole :: l)

  let rec copy ~orig hole =
    let c = Atomic.get orig.callback in
    match c with
    | Filled v -> Loc.set hole v
    | ToFill l ->
      if Atomic.compare_and_set orig.callback c (ToFill (hole :: l)) then
        ()
      else
        copy ~orig hole

  let refine (type a) d v (hs : a Loc.list) : a list =
    set d v;
    match hs with
    | [] ->
      decr d;
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

  let chain (c : 'a P.Computation.t) d =
    (* TODO: Improve efficiency with scheduler support *)
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ ->
        let v = P.Computation.await c in
        set d v;
        decr d;
        ()
      )

  (* Root initialisation *)
  let mk () = 
    let direct_holes = Atomic.make 1 in
    let indirect_counters = Atomic.make 0 in

    let computation = P.Computation.create () in
    let value : 'a ref = ref (Loc.dummy ()) in
    let hole : 'a Loc.t = Loc.mk value 0 in
    let resolve () =
      P.Computation.return computation (Loc.get hole)
    in

    let root = { direct_holes ; indirect_counters ; resolve } in
    let counter = Counter.counter (Root root) 1 in
    let d = create ~hole ~counter ~root in
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
      Destiny.copy ~orig d.hole;
      Destiny.decr d;
      if not (orig.root == d.root) then begin
        Counter.register orig.last_counter d.root
      end

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

  

 
