(* XXX this is still very much a work in progress! Thanks in no small part to
   SML's rather poor support for infix operators *)

structure Bexp =
struct

  open Base
  open Exp
  open ExpOps

  infix 5 $
  infix 5 $$
  infixr 5 \
  infixr 5 \\

  nonfix *
  nonfix +
  nonfix <=
  nonfix <
  
  fun + (e1, e2) = EBinop BPlus $$ #[e1, e2]
  fun * (e1, e2) = EBinop BTimes $$ #[e1, e2]
  fun /\ (e1, e2) = EBinop BAnd $$ #[e1, e2]
  fun \/ (e1, e2) = EBinop BOr $$ #[e1, e2]
  fun <= (e1, e2) = EBinop BLeq $$ #[e1, e2]
  fun < (e1, e2) = EBinop BLt $$ #[e1, e2]

  infix 6 +
  infix 6 \/
  infix 7 *
  infix 7 /\
  infix 5 <=
  infix 5 <

  fun ~ e = EUnop UNeg $$ #[e]
  fun not e = EUnop UNot $$ #[e]

  nonfix @

  fun n x = ENum x $$ #[]
  fun b x = EBool x $$ #[]
  fun eif e1 e2 e3 = EIf $$ #[e1, e2 ,e3]
  fun lambda t (f : Var.t -> Exp.t) =
    let
      val v = Var.newvar ()
    in
      ELam t $$ #[v \\ (f v)]
    end
  fun @ (e1, e2) = EApp $$ #[e1, e2]
  val emp = ENil $$ #[]
  fun cons e1 e2 = ECons $$ #[e1, e2]

  fun choose n es = EChoose (Cont n) $$ #[es]

  fun back n = EBack (Cont n) $$ #[]

  (*
    f3 is uncurried so we can write 
      fn (x, y) => ...
    instead of
      fn x => fn y => ...
  *)
  fun listrec e1 e2 f3 =
    let
      val v1 = Var.newvar ()
      val v2 = Var.newvar ()
    in
      EListRec $$ #[e1, e2, v1 \\ v2 \\ (f3 (v1, v2))]
    end
  fun elet e1 f2 =
    let
      val v = Var.newvar ()
    in
      ELet $$ #[e1, v \\ (f2 v)]
    end
  fun >> (e1, e2) = ESeq $$ #[e1, e2]
  fun send p tp e = ESend (p, tp) $$ #[e]
  fun recv p tp = ERecv (p, tp) $$ #[]

  infix 1 >>
  infix 2 @

end
