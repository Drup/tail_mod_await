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


(** Termination monitor which leverages the GC *)
module Monitor = struct

  type t = { parents : t Saturn.Bag.t } [@@ocaml.unbox]

  let attach m ~parent =
    Saturn.Bag.push m.parents parent

  let root resolve =
    let parents = Saturn.Bag.create () in
    Gc.finalise (fun _ -> resolve ()) parents;
    { parents }

  let node parent =
    let parents = Saturn.Bag.create () in
    Saturn.Bag.push parents parent;
    { parents }
end
  
type 'a status =
  | ToFill of {hole : 'a Loc.t; monitor: Monitor.t; aliases: 'a destiny list}
  | Filled of 'a

and 'h destiny = {
  status : 'h status Atomic.t ;
} [@@ocaml.unbox]

module Destiny = struct

  type 'h t = 'h destiny

  (* Polymorphic map for getting subholes in a destiny *)
  
  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  let create ~monitor ~hole =
    { status = Atomic.make (ToFill {hole; aliases=[]; monitor})  }

  let get_monitor dest =
    match Atomic.get dest.status with
    | Filled _ -> assert false
    | ToFill {monitor;_} -> monitor

  let rec subdestinies
    : type a. _ t -> a Loc.list -> a list
    = fun parent -> function
      | [] -> []
      | hole :: t ->
        let monitor = Monitor.node (get_monitor parent) in
        let d' = create ~monitor ~hole in
        d' :: subdestinies parent t

  (* Actual destiny functions. *)

  let rec set d v =
    let oldv = Atomic.exchange d.status (Filled v) in
    match oldv with
    | Filled v' -> assert (v == v')
    | ToFill {hole; aliases; _} ->
      Loc.set hole v;
      List.iter
        (fun h -> set h (Sys.opaque_identity v))
        aliases;
      Gc.full_major ()

  let rec alias ~original d =
    let c = Atomic.get original.status in
    match c with
    | Filled v -> set d v
    | ToFill {hole; monitor; aliases} ->
      let new_status = ToFill {hole; monitor; aliases = d :: aliases} in
      if Atomic.compare_and_set original.status c new_status then
        Monitor.attach monitor ~parent:(get_monitor d)
      else
        alias ~original d

  let refine (type a) d v (hs : a Loc.list) : a list =
    let subs = subdestinies d hs in
    set d v;
    subs
    
  let fill d v =
    let [] = refine d v [] in ()

  let chain (c : 'a P.Computation.t) d =
    (* TODO: Improve efficiency with scheduler support *)
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ ->
        let v = P.Computation.await c in
        set d v;
        ()
      )

  (* Awaitable initialisation *)
  let mk () = 
    let computation = P.Computation.create () in
    let value : 'a ref = ref (Loc.dummy ()) in
    let hole : 'a Loc.t = Loc.mk value 0 in
    let resolve () =
      P.Computation.return computation (Loc.get hole)
    in
    let monitor = Monitor.root resolve in
    let d = create ~monitor ~hole in
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
    | Found original ->
      (* It was seen. We can register the link *)
      Destiny.alias ~original d;
      (* assert (original.status == d.status); *)
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

  

 
