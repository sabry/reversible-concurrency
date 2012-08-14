(*
  Adding times to the abstract machine in order to make backtracking feasible.

  What do we need times to do?

  1) Create a new time at the beginning of execution
  2) Compare times (probably just leq, but including eq for completeness)
  3) Take the LUB of times when two processes synchronize
    - This operation should return two separate times that are distinct, but
      both know that they're greater than the other time. So it's not quite the
      LUB, I suppose, but it's close. Maybe rename to sync?
    - Wait, maybe not? It's not quite clear.
  4) Increment the time whenever something happens.
*)



signature TIME =
sig

  type t

  val newTime : unit -> t

  val eq : t -> t -> bool
  val leq : t -> t -> bool

  val sync : t -> t -> t

  val tick : t -> t

end
