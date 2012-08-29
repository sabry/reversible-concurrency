structure TypeChecker =
struct

  open Base
  open Exp
  open ExpOps

  infix 5 $
  infix 5 $$

  infixr 4 \
  infixr 4 \\

  type exp = Exp.t

  open EvalUtils

  exception TypeError

  type ctx = tp Context.context

  fun binopType bop t1 t2 =
    case (bop, t1, t2) of
      (BPlus, TInt, TInt) => TInt
    | (BTimes, TInt, TInt) => TInt
    | (BAnd, TBool, TBool) => TBool
    | (BOr, TBool, TBool) => TBool
    | (BLeq, TInt, TInt) => TBool
    | (BLt, TInt, TInt) => TBool
    | _ => raise TypeError

  fun unopType uop t =
    case (uop, t) of
      (UNeg, TInt) => TInt
    | (UNot, TBool) => TBool
    | _ => raise TypeError

  (*
    This also returns a list of the continuations that are initialized, for use
    in the multi-process typechecker. A better abstraction/type system for this
    is almost certainly possible, since this doesn't take into account the
    ability to send functions across channels (for example).
  *)

  fun typecheckExp (ctx : ctx) (e : exp) : tp * (ContSet.t) =
    case out e of
      ` v =>
        (case Context.lookup ctx v of
          SOME tp => (tp, [])
        | NONE => raise TypeError)
    | ENum _ $ #[] => (TInt, ContSet.empty)
    | EBool _ $ #[] => (TBool, ContSet.empty)
    | EBinop bop $ #[e1, e2] =>
        let
          val (t1, cs1) = typecheckExp ctx e1
          val (t2, cs2) = typecheckExp ctx e2
          val cs = ContSet.union cs1 cs2
        in
          (binopType bop t1 t2, cs)
        end
    | EUnop uop $ #[e] =>
        let
          val (t, cs) = typecheckExp ctx e
        in
          (unopType uop t, cs)
        end
    | EIf $ #[eb, et, ef] =>
        let
          val (tb, csb) = typecheckExp ctx eb
          val (tt, cst) = typecheckExp ctx et
          val (tf, csf) = typecheckExp ctx ef
          val cs = ContSet.union (ContSet.union csb cst) csf
        in
          case tb of
            TBool =>
              if tt = tf
              then (tt, cs)
              else raise TypeError
        end
    | ELam t $ #[xe] =>
        (case out xe of
          x \ e =>
            let
              val (t', cs) = typecheckExp (Context.insert ctx x t) e
            in
              (TArr (t, t'), cs)
            end)
    | EApp $ #[e1, e2] =>
        let
          val (t1, cs1) = typecheckExp ctx e1
          val (t2, cs2) = typecheckExp ctx e2
          val cs = ContSet.union cs1 cs2
        in
          case t1 of
            TArr (t, t') =>
              if t = t2
              then (t', cs)
              else raise TypeError
          | _ => raise TypeError
        end
    | ENil t $ #[] => (TList t, [])
    | ECons $ #[e1, e2] =>
        let
          val (t1, cs1) = typecheckExp ctx e1
          val (t2, cs2) = typecheckExp ctx e2
          val cs = ContSet.union cs1 cs2
        in
          case t2 of
            TList t =>
              if t = t1
              then (t2, cs)
              else raise TypeError
          | _ => raise TypeError
        end
    | EListRec $ #[el, en, xyec] =>
        let
          val (tl, csl) = typecheckExp ctx el
          val (tn, csn) = typecheckExp ctx en
          val cs = ContSet.union csl csn
        in
          case tl of
            TList t =>
              (case out2 xyec of
                x \ (y \ e) =>
                  let
                    val (tc, csc) = 
                      typecheckExp
                        (Context.insert 
                          (Context.insert ctx x t) y tn)
                        e
                    val cs = ContSet.union cs csc
                  in
                    if tc = tn
                    then (tc, cs)
                    else raise TypeError
                  end)
          | _ => raise TypeError
        end
    | ELet $ #[e, xe'] =>
        let
          val (t, cs) = typecheckExp ctx e
        in
          case out xe' of
            x \ e' =>
              let
                val (t', cs') = typecheckExp (Context.insert ctx x t) e'
              in
                (t', ContSet.union cs cs')
              end
        end
    | ESeq $ #[e1, e2] =>
        let
          val (t1, cs1) = typecheckExp ctx e1
          val (t2, cs2) = typecheckExp ctx e2
          val cs = ContSet.union cs1 cs2
        in
          (t2, cs)
        end
    | ESend (_, t) $ #[e] =>
        let
          val (t', cs) = typecheckExp ctx e
        in
          if t' = t
          then (TInt, cs)
          else raise TypeError
        end
    | ERecv (_, t) $ #[] => (t, [])
    | EChoose c $ #[e] =>
        let
          val (t, cs) = typecheckExp ctx e
        in
          case t of
            TList t' => (t', ContSet.insert cs c)
          | _ => raise TypeError
        end
    | EBack c $ #[] => (TInt, [])
    
end
