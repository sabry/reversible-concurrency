signature TYPECHECKER =
sig
  structure Exp     : ABTUTIL
  structure Type    : ABTUTIL
  structure Context : CONTEXT where type Key.ord_key = Var.t

  val typecheck : Type.t Context.context -> Exp.t -> Type.t
  val equiv : Type.t -> Type.t -> bool
end
