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
}

and 'a dangling_link =
  { first : 'a Loc.t ; late : 'a Loc.t }

and ('k, 'a) memo = {
  tbl : ('k, 'a destiny) H.t ;
  dangling_links : 'a dangling_link St.t ;
}

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
  let oldv = Atomic.fetch_and_add r.direct_holes n in
  (* let newv = oldv + n in *)
  (* assert (newv >= 0); *)
  root_try_resolve r

let root_incr_indirect r =
  Atomic.incr r.indirect_counters
let root_decr_indirect r = 
  Atomic.decr r.indirect_counters;
  root_try_resolve r

let apply_links m =
  (* TODO Check atomicity here *)
  let l = St.pop_all m.dangling_links in
  List.iter (fun {first = first; late = late} ->
      Loc.set late (Loc.get first)
    ) l;
  ()

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
    let oldv = Atomic.fetch_and_add c.nb n in
    let newv = oldv + n in
    assert (newv >= 0);
    begin match c.parent with
      | Root r -> root_update_direct r n
      | Counter c -> update c n
    end;
    checkif0 c newv

end

module Destiny = struct

  type 'h t = 'h destiny

  (* Polymorphic map for getting subholes in a destiny *)
  
  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  let rec subdestinies
    : type a. _ t -> a Loc.list -> a list
    = fun d -> function
      | [] -> []
      | hole :: t ->
        let counter = Counter.counter (Counter d.last_counter) 1 in
        let d' = { d with hole ; last_counter = counter } in
        d' :: subdestinies d t

  let decr d =
    Counter.update d.last_counter (-1)
  let incr d n =
    Counter.update d.last_counter n

  (* Actual destiny functions. *)

  let set d v =
    Loc.set d.hole (Sys.opaque_identity v)
  
  let refine (type a) d v (hs : a Loc.list) : a list =
    set d v;
    match hs with
    | [] ->
      decr d;
      []
    | [ hole ] ->
      (* Only one hole. No need to update counter. *)
      [{ d with hole }]
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
  let mk tbl = 
    let direct_holes = Atomic.make 1 in
    let indirect_counters = Atomic.make 0 in

    let computation = P.Computation.create () in
    let value = ref (Loc.dummy ()) in
    let hole = Loc.mk value 0 in
    let resolve () =
      apply_links tbl;
      P.Computation.return computation !value
    in

    let root = { direct_holes ; indirect_counters ; resolve } in
    let counter = Counter.counter (Root root) 1 in
    let d = { hole; last_counter = counter; root } in
    d, computation
end

module Memo = struct

  type ('k, 'a) t = ('k, 'a) memo

  let init () =
    let tbl = H.create () in
    let dangling_links = St.create () in
    { tbl ; dangling_links }

  let memo (m : _ t) d key f =
    (* Atomically check if [key] has been seen. *)
    if H.try_add m.tbl key d then
      (* It wasn't seen, we have the lock. We can execute f *)
      f key
    else begin
      (* It was seen. We can register the link *)
      let dfirst = H.find_exn m.tbl key in
      St.push m.dangling_links {first = dfirst.hole ; late = d.hole};
      Destiny.decr d;
      if dfirst.root == d.root then
        ()
      else begin
        Counter.register dfirst.last_counter d.root
      end
    end

end



module AsyncFun = struct

  let fork f =
    let c = P.Computation.create () in
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ -> f(); P.Computation.finish c)

end

  

 
