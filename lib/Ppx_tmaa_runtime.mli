module Hole : sig
  (** Hole inside a computation *)
  
  type 'h t =
    | Obj : 'h t
    | Field : int * 'h t -> 'h t

  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  (** [hole p] is the value to insert at position [p] to act like a hole. *)
  val hole : 'h t -> 'h

end

module Destiny : sig
  (** The destiny of an asynchronous computation. *)
  
  (** A Destiny whose current hole is of type ['a] *)
  type -'a t

  (** An heterogeneous list of destinies *)
  type 'a list =
    | [] : unit list
    | (::) : 'h t * 'h2 list -> ('h * 'h2) list

  (** [create ()] returns a synchronised destiny and promise.
      The promise will be fulfilled only when all holes in the destiny are
      filled.
  *)
  val create : unit -> 'a t * 'a Picos.Computation.t

  (** [fill d v hs] fill the destiny [d] with a value [v]
      that currently has the holes [hs].
      It returns a list of destinies corresponding to each holes.
      [d] should not be used again.
  *)
  val refine : 'b t -> 'b -> 'a Hole.list -> 'a list

  (** [fill d v] is an alias for [refine d v []]. *)
  val fill : 'b t -> 'b -> unit

  (** [forward d c] will fill the destiny [d] with the value of [c]
      when it is finished. *)
  val forward : 'a t -> 'a Picos.Computation.t -> unit
end

module AsyncFun : sig

  type t

  val fork : (unit -> unit) -> unit

end
