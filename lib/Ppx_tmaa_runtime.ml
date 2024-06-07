module Destiny : sig
  (** The destiny of an asynchronous computation. *)
  
  type 'h hole =
    | Obj : 'h hole
    | Field : int * 'h hole -> 'h hole
  type 'a hlist =
    | [] : unit hlist
    | (::) : 'h hole * 'h2 hlist -> ('h * 'h2) hlist

  (** [hole p] is the value to insert at position [p] to act like a hole. *)
  val hole : 'h hole -> 'h
  
  (** A Destiny whose current hole is of type ['a] *)
  type -'a t

  (** An heterogeneous list of destinies *)
  type 'a dlist =
    | [] : unit dlist
    | (::) : 'h t * 'h2 dlist -> ('h * 'h2) dlist

  (** [create ()] returns a synchronised destiny and promise.
      The promise will be fulfilled only when all holes in the destiny are
      filled.
  *)
  val create : ('a -> unit) -> 'a t

  (** [fill d v hs] fill the destiny [d] with a value [v]
      that currently has the holes [hs].
      It returns a list of destinies corresponding to each holes.
      [d] should not be used again.
  *)
  val refine : 'b t -> 'b -> 'a hlist -> 'a dlist

  (** [fill d v] is an alias for [refine d v []]. *)
  val fill : 'b t -> 'b -> unit
end = struct

  type 'h hole =
    | Obj : 'h hole
    | Field : int * 'h hole -> 'h hole

  type 'a hlist =
    | [] : unit hlist
    | (::) : 'h hole * 'h2 hlist -> ('h * 'h2) hlist

  module Hole = struct
    let rec length : type a . a hlist -> int = function
      | [] -> 0
      | _ :: t -> length t + 1

    let hash (p : _ hole) = Hashtbl.hash p

    (** [hole p] gives the value to insert at position [p] to act like
        a hole. *)
    let hole p0 =
      let rec aux : type a b . a hole -> a = function
        | Obj -> Obj.magic @@ ref (hash p0)
        | Field (_i, a) -> aux a
      in
      aux p0

    let rec set_with_checksum
      : type a . checksum:a -> Obj.t -> a hole -> a -> unit
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

  let hole = Hole.hole
  
  type -'h t =
      Destiny : {
        counter : int Atomic.t ;
        resolve : unit -> unit ;
        set : 'h -> unit ;
      } -> 'h t

  type 'a dlist =
    | [] : unit dlist
    | (::) : 'h t * 'h2 dlist -> ('h * 'h2) dlist

  type f = { f : 'a . 'a hole -> 'a t }
  let rec map_holes
    : type a. f -> a hlist -> a dlist
    = fun f -> function
      | [] -> []
      | h :: t -> f.f h :: map_holes f t
  
  let create k =
    let counter = Atomic.make 1 in
    let path = Field (0, Obj) in
    let root = ref (Hole.hole path) in
    let resolve () = k !root in
    let set v = Hole.set (Obj.repr root) path v in
    let d = Destiny { counter; resolve; set } in
    d
  
  let refine (type a) (Destiny d) v (hs : a hlist) : a dlist =
    assert (Atomic.get d.counter >= 1);
    d.set v;
    match hs with
    | [] ->
      Atomic.decr d.counter;
      if Atomic.compare_and_set d.counter 0 (-1) then
        d.resolve ();
      []
    | _ -> 
      let holes = Hole.length hs in
      let _ = Atomic.fetch_and_add d.counter (- 1 + holes) in
      map_holes
        { f = fun hole ->
              let set x = Hole.set (Obj.repr v) hole x in
              Destiny {d with set } }
        hs
  let fill d v =
    let [] = refine d v [] in ()
end
