signature VAR =
sig

  type t

  val newvar : unit -> t
  val named : string -> t

  val eq : t -> t -> bool
  val compare : t * t -> order

  val toString : t -> string

end
