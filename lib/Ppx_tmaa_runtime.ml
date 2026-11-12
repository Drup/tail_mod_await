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


  (* A fiber requesting the scheduler to trigger a major collection.
     There should always be only one such fiber.
  *)  
  type gc_request = Sleeping | Scheduled
  let gc_request_status = Atomic.make Sleeping
  let request_gc () =
    let oldv = Atomic.exchange gc_request_status Scheduled in
    match oldv with
    | Scheduled -> () (* The fiber was already created *)
    | Sleeping ->
      fork (fun _ ->
          Gc.full_major ();
          Atomic.set gc_request_status Sleeping
        )

end

type 'a status =
  | Unaliased of {hole : 'a Loc.t; monitor: Monitor.t}
  | Aliasable of {holes : 'a Loc.t list; monitor: Monitor.t}
  | Alias
  | Filled of 'a

and 'a destiny = {
  status : 'a status Atomic.t ;
} [@@ocaml.unbox]

module Destiny = struct

  type 'h t = 'h destiny

  (* Polymorphic map for getting subholes in a destiny *)
  
  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  let create ~monitor ~hole =
    { status = Atomic.make (Unaliased {hole; monitor})  }

  let get_monitor dest =
    match Atomic.get dest.status with
    | Filled _ -> assert false
    | Aliasable {monitor; _} -> monitor
    | Unaliased {monitor; _} -> monitor
    | Alias -> assert false

  let rec subdestinies
    : type a. parent:Monitor.t -> a Loc.list -> a list
    = fun ~parent -> function
      | [] -> []
      | hole :: t ->
        let monitor = Monitor.node parent in
        let d' = create ~monitor ~hole in
        d' :: subdestinies ~parent t

  (* Actual destiny functions. *)

  let set d v =
    let oldv = Atomic.exchange d.status (Filled v) in
    match oldv with
    | Filled v' -> assert (v == v')
    | Alias -> assert false
    | Aliasable {holes; _} ->
      List.iter
        (fun hole -> Loc.set hole (Sys.opaque_identity v))
        holes
    | Unaliased {hole; _} ->
      Loc.set hole (Sys.opaque_identity v)

  (* A direct fill.
     Since the numer of hole strictly decreases, we may check termination. *)
  let fill d v =
    set d v;
    AsyncFun.request_gc ()

  let refine (type a) d v (hs : a Loc.list) : a list =
    match hs with
    | [] -> fill d v; []
    | [ hole ] ->
      (* No need to change monitor for direct edges. *)
      let d' = create ~monitor:(get_monitor d) ~hole in
      set d v;
      [ d' ]
    | _ ->
      let subs = subdestinies ~parent:(get_monitor d) hs in
      set d v;
      subs
  
  let update_alias d =
    let oldst = Atomic.exchange d.status Alias in
    match oldst with
    | Unaliased {hole; monitor} -> hole, monitor
    | _ -> assert false
  
  let rec alias ~original d =
    (* [alias d] is only possible if [d] us [Unaliased]. *)
    let dhole, dmonitor = update_alias d in
    let c = Atomic.get original.status in
    match c with
    | Filled v -> set d v
    | Alias -> assert false
    | Unaliased {hole; monitor} ->
      let new_status = Aliasable {holes = [hole; dhole]; monitor} in
      if Atomic.compare_and_set original.status c new_status then
        Monitor.attach monitor ~parent:dmonitor
      else
        alias ~original d
    | Aliasable {holes; monitor} ->
      let new_status = Aliasable {holes = dhole :: holes; monitor} in
      if Atomic.compare_and_set original.status c new_status then
        Monitor.attach monitor ~parent:dmonitor
      else
        alias ~original d


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



  

 
