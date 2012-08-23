signature DISJOINT_SET =
sig

  structure O : ORD
  exception Repeat of O.t

  type t

  val empty : t

  val insert : t -> O.t -> t
  val singleton : O.t -> t

  val union : t -> t -> t (* fails if not disjoint *)

end
