signature ABT =
sig
  structure O : OPER

  type t

  datatype 'a view =
    ` of Var.t
  | \ of Var.t * 'a
  | $ of O.t * 'a vector

  val map : ('a -> 'b) -> 'a view -> 'b view

  val into : t view -> t
  val out  : t -> t view

  val aequiv : t -> t -> bool
end
