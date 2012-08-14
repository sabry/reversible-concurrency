signature EVALUATOR =
sig
  structure Exp : ABTUTIL

  val evaluate : Exp.t -> Exp.t
  val equiv : Exp.t -> Exp.t -> bool
end
