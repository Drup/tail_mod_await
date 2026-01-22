module P = Picos

module Loc = struct

  type 'h t = {
    pointer : Obj.t ;
    offset : int ;
  }

  type loc = Loc : 'h t -> loc
  
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

module Root = struct
  module H = Saturn.Htbl
  module St = Saturn.Stack

  type dangling_link =
    { first : Loc.loc ; late : Loc.loc }
  
  type ('k, 'a) t = Root of {
    nb_holes : int Atomic.t ;
    tbl : ('k, Loc.loc) H.t ;
    dangling_links : dangling_link St.t ;
    computation : 'a P.Computation.t ;
    loc : 'a Loc.t ;
  }

  let init () = 
    let computation = P.Computation.create () in
    let tbl = H.create () in
    let nb_holes = Atomic.make 1 in
    let value = ref (Loc.dummy ()) in
    let loc = Loc.mk value 0 in
    let dangling_links = St.create () in
    Root { tbl ; nb_holes ; dangling_links ; loc ; computation }

  let as_loc (Root r) = r.loc
  let as_promise (Root r) = r.computation

  let resolve (Root r) =
    begin if St.is_empty r.dangling_links then
        Format.eprintf "No dangling links.@."
      else
        let l = St.pop_all r.dangling_links in
        List.iter (fun {first = Loc first; late = Loc late} ->
            Loc.set late (Obj.magic @@ Loc.get first)
          ) l;
    end;
    P.Computation.return r.computation (Loc.get r.loc)

  let add_holes (Root r) n =
    assert (n > 0);
    let _ = Atomic.fetch_and_add r.nb_holes (- 1 + n) in
    ()
      
  let[@inline always] decr_holes (Root r as r0) = 
    Atomic.decr r.nb_holes;
    if Atomic.compare_and_set r.nb_holes 0 (-1) then
      resolve r0
  
  let memo (Root r as r0 : _ t) key loc f =
    (* Atomically check if [key] has been seen. *)
    if H.try_add r.tbl key (Loc loc) then
      (* It wasn't seen, we have the lock. We can execute f *)
      f key
    else
      (* It was seen. We can register the link *)
      let first = H.find_exn r.tbl key in
      St.push r.dangling_links {first; late = Loc loc};
      decr_holes r0;
end


module Destiny = struct

  type ('k, 'h) t = D : {
      hole : 'h Loc.t;
      root : ('k, 'r) Root.t ;
    } -> ('k, 'h) t

  let mk () =
    let root = Root.init () in
    let hole = Root.as_loc root in
    D { hole ; root }, Root.as_promise root

  (* Polymorphic map for getting subholes in a destiny *)
  
  type ('k, 'a) list =
    | [] : ('k, unit) list
    | (::) : ('k, 'h) t * ('k, 'h2) list -> ('k, 'h * 'h2) list

  let rec subdestinies
    : type a. _ t -> a Loc.list -> (_, a) list
    = fun (D d as d0) -> function
      | [] -> []
      | hole :: t -> D {d with hole} :: subdestinies d0 t

  (* Memoisation *)
  let memo (D d) key f =
    Root.memo d.root key d.hole f

  (* Actual destiny functions. *)

  let set (D d) v =
    Loc.set d.hole (Sys.opaque_identity v)

  let refine (type a) (D d as d0) v (hs : a Loc.list) : (_, a) list =
    set d0 v;
    match hs with
    | [] ->
      Root.decr_holes d.root;
      []
    | _ -> 
      let locs = Loc.length hs in
      Root.add_holes d.root locs;
      subdestinies d0 hs

  let fill d v =
    let [] = refine d v [] in ()

  let chain (c : 'a P.Computation.t) (D d as d0) =
    (* TODO: Improve efficiency with scheduler support *)
    let fiber = P.Fiber.create ~forbid:false c in
    P.Fiber.spawn fiber (fun _ ->
        let v = P.Computation.await c in
        set d0 v;
        Root.decr_holes d.root;
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

  

 
