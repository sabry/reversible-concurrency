functor AbtFn (Oper : OPER) :> ABT where type O.t = Oper.t =
struct

  infix 5 \
  infix 5 $

  structure O : OPER = Oper
  structure V : VAR = Var

  fun uncurry f (x,y) = f x y

  datatype t =
    FREE of Var.t
  | BOUND of int
  | ABS of t
  | APP of Oper.t * t vector

  datatype 'a view =
    ` of Var.t
  | \ of Var.t * 'a
  | $ of Oper.t * 'a vector

  fun map f e =
    case e of
      ` v => ` v
    | v \ e' => v \ f e'
    | p $ es => p $ Vector.map f es

  fun addvar v n e =
    case e of
      FREE v' => e
    | BOUND m => if m = n then FREE v else e
    | ABS e' => ABS (addvar v (n + 1) e')
    | APP (p, es) => APP (p, Vector.map (addvar v n) es)

  fun out e =
    case e of
      FREE v => ` v
    | BOUND n => raise Fail "bound variable occured in out"
    | ABS e' =>
      let
        val v = Var.newvar ()
      in
        v \ addvar v 0 e'
      end
    | APP (p, es) => p $ es

  fun shiftvar v n e =
    case e of
      FREE v' =>
        if Var.eq v v' then BOUND n else e
    | BOUND m => e
    | ABS e' => ABS (shiftvar v (n + 1) e')
    | APP (p, es) => APP (p, Vector.map (shiftvar v n) es)

  fun match_arity (n, e) = 
    case (n, e) of
      (0, ABS _)  => false
    | (0, _)      => true
    | (n, ABS e') => match_arity (n - 1, e')
    | (n, _)      => false

  fun doapp (oper, es) =
    if VectorUtil.pairAllEq match_arity (O.arity oper) es
    then APP (oper, es)
    else raise Fail "bad arity"

  fun into e =
    case e of
      ` v => FREE v
    | v \ e' => ABS (shiftvar v 0 e')
    | p $ es => doapp (p, es)

  fun aequiv e1 e2 =
    case (e1, e2) of
      (FREE v1, FREE v2) => Var.eq v1 v2
    | (BOUND m, BOUND n) => m = n
    | (ABS e1', ABS e2') => aequiv e1' e2'
    | (APP (p1, es1), APP (p2, es2)) =>
        O.eq p1 p2 andalso VectorUtil.pairAllEq (uncurry aequiv) es1 es2
    | _ => false

end
