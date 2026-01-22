module Loc : sig
  (** Loc inside a block *)
  
  type 'h t

  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  (** A dummy value to be inserted into blocks. *)
  val dummy : unit -> 'a
  
  (** [Loc.mk o i] is the location in value [o] at offset [i].

      [o] {i must} b a pointer to a block. *)
  val mk : 'a -> int -> 'b t

  (** [get l] gets the value at location [l].

      You should make sure it has been filled.
  *)
  val get : 'a t -> 'a
end

module Destiny : sig
  (** The destiny of an asynchronous computation. *)
  
  (** A Destiny whose current hole is of type ['a] and whose memoisation
      uses type 'k *)
  type ('k, 'h) t

  (** An heterogeneous list of destinies *)
  type ('k, 'a) list =
    | [] : ('k, unit) list
    | (::) : ('k, 'h) t * ('k, 'h2) list -> ('k, 'h * 'h2) list

  (** [mk ()] returns a synchronised destiny and promise.
      The promise will be fulfilled only when all holes in the destiny are
      filled.
  *)
  val mk : unit -> ('k, 'a) t * 'a Picos.Computation.t

  (** [refine d v hs] fill the destiny [d] with a value [v]
      that currently has the holes [hs].
      It returns a list of destinies corresponding to each holes.
      [d] should not be used again.
  *)
  val refine : ('k, 'b) t -> 'b -> 'a Loc.list -> ('k, 'a) list

  (** [fill d v] is an alias for [refine d v []]. *)
  val fill : ('k, 'b) t -> 'b -> unit

  (** [chain c d] will fill the destiny [d] with the value of the
      asynchronous computation [c] when it is finished. *)
  val chain : 'a Picos.Computation.t -> ('k, 'a) t -> unit

  val memo : ('a, 'b) t -> 'a -> ('a -> unit) -> unit
end

module AsyncFun : sig

  type t

  val fork : (unit -> unit) -> unit

end
