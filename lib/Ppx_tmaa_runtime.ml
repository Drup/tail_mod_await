module P = Picos

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

  let dummy = Obj.magic @@ ref ()
      
  let is_uninitialized loc =
    Obj.field loc.pointer loc.offset == dummy

  (** [set l v] fills the location [l] with [v].
      It verifies that [l] was never filled.
  *)
  let set
    : type a . a t -> a -> unit
    = fun loc v ->
      assert (is_uninitialized loc);
      let tag = Obj.tag loc.pointer in
      if tag = Obj.double_array_tag then
        Obj.set_double_field loc.pointer loc.offset (Obj.magic v)
      else
        Obj.set_field loc.pointer loc.offset (Obj.repr v)

end

module Destiny = struct
  
  type -'h t =
      Destiny of {
        counter : int Atomic.t ;
        resolve : unit -> unit ;
        set : 'h -> unit ;
      }

  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  type f = { f : 'a . 'a Loc.t -> 'a t }
  let rec map_holes
    : type a. f -> a Loc.list -> a list
    = fun f -> function
      | [] -> []
      | h :: t -> f.f h :: map_holes f t
  
  let mk () =
    let c = P.Computation.create () in
    let counter = Atomic.make 1 in
    let root = ref (Obj.obj Loc.dummy) in
    let resolve () = P.Computation.return c !root in
    let set v = assert (!root == Obj.obj Loc.dummy); root := v in
    let d = Destiny { counter; resolve; set } in
    d, c

  let[@inline always] decr_and_check (Destiny d) = 
    Atomic.decr d.counter;
    if Atomic.compare_and_set d.counter 0 (-1) then
      d.resolve ()

  let refine (type a) (Destiny r as d) v (hs : a Loc.list) : a list =
    assert (Atomic.get r.counter >= 1);
    r.set (Sys.opaque_identity v);
    match hs with
    | [] ->
      decr_and_check d;
      []
    | _ -> 
      let locs = Loc.length hs in
      let _ = Atomic.fetch_and_add r.counter (- 1 + locs) in
      map_holes
        { f = fun loc ->
              let set x = Loc.set loc x in
              Destiny {r with set} }
        hs

  let fill d v =
    let [] = refine d v [] in ()

  let chain (c : 'a P.Computation.t) (Destiny r as d) =
    (* TODO: This doesn't look terribly efficient. *)
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ ->
        let v = P.Computation.await c in
        r.set v;
        decr_and_check d;
        ()
      )
end

module AsyncFun = struct

  type t = P.Fiber.t

  let fork f =
    let c = P.Computation.create () in
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ -> f(); P.Computation.finish c)

end

  

 
