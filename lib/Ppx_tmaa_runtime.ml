open Picos

module Hole = struct
  
  type 'h t =
    | Obj : 'h t
    | Field : int * 'h t -> 'h t

  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list
  
  let rec length : type a . a list -> int = function
    | [] -> 0
    | _ :: t -> length t + 1

  let hash (p : _ t) = Hashtbl.hash p

  (** [hole p] gives the value to insert at position [p] to act like
      a hole. *)
  let hole p0 =
    let rec aux : type a b . a t -> a = function
      | Obj -> Obj.magic @@ ref (hash p0)
      | Field (_i, a) -> aux a
    in
    aux p0

  let rec set_with_checksum
    : type a . checksum:a -> Obj.t -> a t -> a -> unit
    = fun ~checksum o p v ->
      match p with
      | Obj -> assert false
      (* | Field (i, Double) -> *)
      (*   assert (Obj.double_field o i = checksum); *)
      (*   Obj.set_double_field o i v *)
      (* | Field (i, Immediate) -> *)
      (*   assert (Obj.field o i = Obj.repr checksum); *)
      (*   Obj.set_field o i (Obj.repr v) *)
      | Field (i, Obj) ->
        assert (Obj.field o i = Obj.repr checksum);
        Obj.set_field o i (Obj.repr v)
      | Field (i, p) ->
        let o' = Obj.field o i in set_with_checksum ~checksum o' p v

  (** [set o p v] fills the hole at position [p] in [o] with [v].
      Checks that the current content at [p] in [o] was never filled, and
      is still a hole (built with {!hole}).
  *)
  let set o p v = set_with_checksum ~checksum:(hole p) o p v
end

module Destiny = struct
  
  type -'h t =
      Destiny : {
        counter : int Atomic.t ;
        resolve : unit -> unit ;
        set : 'h -> unit ;
      } -> 'h t

  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  type f = { f : 'a . 'a Hole.t -> 'a t }
  let rec map_holes
    : type a. f -> a Hole.list -> a list
    = fun f -> function
      | [] -> []
      | h :: t -> f.f h :: map_holes f t
  
  let create () =
    let c = Computation.create () in
    let counter = Atomic.make 1 in
    let path = Hole.Field (0, Obj) in
    let root = ref (Hole.hole path) in
    let resolve () = Computation.return c !root in
    let set v = Hole.set (Obj.repr root) path v in
    let d = Destiny { counter; resolve; set } in
    d, c

  let[@inline always] decr_and_check (Destiny d) = 
    Atomic.decr d.counter;
    if Atomic.compare_and_set d.counter 0 (-1) then
      d.resolve ()

  let refine (type a) (Destiny r as d) v (hs : a Hole.list) : a list =
    assert (Atomic.get r.counter >= 1);
    r.set v;
    match hs with
    | [] ->
      decr_and_check d;
      []
    | _ -> 
      let holes = Hole.length hs in
      let _ = Atomic.fetch_and_add r.counter (- 1 + holes) in
      map_holes
        { f = fun hole ->
              let set x = Hole.set (Obj.repr v) hole x in
              Destiny {r with set} }
        hs
  let fill d v =
    let [] = refine d v [] in ()

  let forward (Destiny r as d) (c : 'a Computation.t) =
    (* TODO: This doesn't look terribly efficient. *)
    let fiber = Fiber.create ~forbid:false c in
    Fiber.spawn fiber (fun _ ->
        let v = Computation.await c in
        decr_and_check d;
        r.set v
      )
end

module AsyncFun = struct

  type t = Fiber.t

  let fork f =
    let c = Computation.create () in
    let fiber = Fiber.create ~forbid:false c in
    Fiber.spawn fiber (fun _ -> f(); Computation.finish c)

end

  

 
