functor AbtUtil (A : ABT) : ABTUTIL =
struct
  
  open A

  infix 5 $
  infix 5 $$
  infix 5 \
  infix 5 \\

  fun `` v    = into (` v)
  fun v \\ e  = into (v \ e)
  fun p $$ es = into (p $ es)

  fun subst e v e' =
    case out e' of
      ` v' => if Var.eq v v' then e else e'
    | v' \ e'' => v' \\ subst e v e''
    | p $ es => p $$ Vector.map (subst e v) es

  fun toString e =
    case out e of
      ` v => Var.toString v
    | v \ e => (Var.toString v) ^ "." ^ (toString e)
    | p $ es => (O.toString p) ^ " $ " ^ (VectorUtil.toString toString es)

end
