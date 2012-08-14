signature OPER =
sig

  type t

  val eq : t -> t -> bool
  val arity : t -> int vector

  val toString : t -> string

end
