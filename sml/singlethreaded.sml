structure SingleThreadedEvaluator =
struct
  
  open Base
  open Exp
  open ExpOps

  infix 5 $
  infix 5 $$

  infixr 4 \
  infixr 4 \\

  type exp = Exp.t
  type time = Time.t

  val tick = Time.tick

  open EvalUtils

  exception Stuck

  (*
    Evaluating *only* looks at an expression, not the context in which that
    expression is evaluating, so there must be some way of adding whatever
    context is created to the rest of the context.

    In other words, eval is monadic, and buildConfig does part of the work of
    bind.
  *)

  fun buildConfig s1 (CEval (MkEval (Mem s2, e2))) =
        ceval (Mem (s2 @ s1), e2)
    | buildConfig s1 (CRetn (MkRetn (Mem s2, e2))) =
        cretn (Mem (s2 @ s1), e2)
    | buildConfig s1 (CSend (MkSend (Mem s2, e2))) =
        csend (Mem (s2 @ s1), e2)
    | buildConfig s1 (CRecv (MkRecv (Mem s2, e2))) =
        crecv (Mem (s2 @ s1), e2)
    | buildConfig s1 (CBack (MkBack (Mem s2, e2))) =
        cback (Mem (s2 @ s1), e2)
    (* XXX nonexhaustive *)

  (*
    runBinop and runUnop are helper functions that abstract out the job of
    evaluating binaro and unary expressions to their values.
  *)

  fun runBinop bop e1 e2 =
    case (bop, out e1, out e2) of
      (BPlus, ENum n1 $ #[], ENum n2 $ #[]) => ENum (n1 + n2) $$ #[]
    | (BTimes, ENum n1 $ #[], ENum n2 $ #[]) => ENum (n1 * n2) $$ #[]
    | (BAnd, EBool b1 $ #[], EBool b2 $ #[]) => EBool (b1 andalso b2) $$ #[]
    | (BOr, EBool b1 $ #[], EBool b2 $ #[]) => EBool (b1 orelse b2) $$ #[]
    | (BLeq, ENum n1 $ #[], ENum n2 $ #[]) => EBool (n1 <= n2) $$ #[]
    | (BLt, ENum n1 $ #[], ENum n2 $ #[]) => EBool (n1 < n2) $$ #[]

  fun runUnop uop e =
    case (uop, out e) of
      (UNeg, ENum n $ #[]) => ENum (~n) $$ #[]
    | (UNot, EBool b $ #[]) => EBool (not b) $$ #[]

  (*
    step calls eval or retn, based on the state that the code is in. All three
    of these functions are single-threaded; that is, they act independent of
    any surrounding state.
   *)

  (* eval : exp -> config *)

  fun eval e =
    case (out e) of
      ENum n $ #[] => cretn (Mem [], e)
    | EBool b $ #[] => cretn (Mem [], e)
    | EIf $ #[eb, et, ef] => ceval (Mem [FIf (et, ef)], eb)
    | EBinop bop $ #[e1, e2] => ceval (Mem [FBinopL (bop, e2)], e1)
    | EUnop uop $ #[e'] => ceval (Mem [FUnop uop], e')
    | ELam t $ #[xe] => cretn (Mem [], e)
    | EApp $ #[e1, e2] => ceval (Mem [FAppL e2], e1)
    | ENil _ $ #[] => cretn (Mem [], e)
    | ECons $ #[e1, e2] => ceval (Mem [FConsL e2], e1)
    | EListRec $ #[el, en, ec] => ceval (Mem [FListRec (en, ec)], el)
    | ELet $ #[e1, e2] => ceval (Mem [FLet e2], e1)
    | ESeq $ #[e1, e2] => ceval (Mem [FSeq e2], e1)
    | ESend ptp $ #[e'] => ceval (Mem [FSend ptp], e')
    | ERecv ptp $ #[] => crecv (Mem [], ptp)
    | EChoose k $ #[e'] => ceval (Mem [FChoose k], e')
    | EBack k $ #[] => cback (Mem [], k)


  (* retn : mem -> exp -> config *)

  fun retn (f :: s) e =
    case f of
      FBinopL (bop, e2) => ceval (Mem (FBinopR (bop, e) :: s), e2)
    | FBinopR (bop, e1) => cretn (Mem s, runBinop bop e1 e)
    | FIf (et, ef) =>
        (case out e of
          EBool true $ #[] => ceval (Mem s, et)
        | EBool false $ #[] => ceval (Mem s, ef))
    | FUnop uop => cretn (Mem s, runUnop uop e)
    | FAppL e2 => ceval (Mem (FAppR e :: s), e2)
    | FAppR e1 =>
        (case out2 e1 of
          ELam tp $ #[x \ b] => ceval (Mem s, subst e x b))
    | FConsL e2 => ceval (Mem (FConsR e :: s), e2)
    | FConsR e1 => cretn (Mem s, ECons $$ #[e1, e])
	  | FListRec (en, ec) =>
        (case out e of
          ENil _ $ #[] => ceval (Mem s, en)
        | ECons $ #[head, tail] =>
            (case out2 ec of
              x \ (y \ ec') =>
                ceval (Mem s, subst head x
                                (subst (EListRec $$ #[tail, en, ec]) y ec'))))
	  | FLet xe' =>
        (case out xe' of
          x \ e' => ceval (Mem s, subst e x e'))
	  | FSeq e2 => ceval (Mem s, e2)
	  | FSend (p, tp) => csend (Mem s, (p, tp, e))
    | FChoose k => ctime (k, Mem s, e)
  
  (* step : config -> config, only works if a step can be taken *)

  fun step (CEval (MkEval (Mem s, e))) = buildConfig s (eval e)
    | step (CRetn (MkRetn (Mem s, e))) = retn s e

  fun canStepAlone (CEval _) = true
    | canStepAlone (CRetn (MkRetn (Mem (_ :: _), _))) = true
    | canStepAlone _ = false

  fun runProgram e = if canStepAlone e then runProgram (step e) else e

end
