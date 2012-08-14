signature ABTUTIL =
sig
  
  include ABT

  val `` : Var.t -> t
  val \\ : Var.t * t -> t
  val $$ : O.t * t vector -> t

  val subst : t -> Var.t -> t -> t
  val toString : t -> string

end
