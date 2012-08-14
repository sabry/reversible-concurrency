structure Conc =
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

  datatype frame =
    FBinopL of binop * exp | FIf of exp * exp | FBinopR of binop * exp
  | FUnop of unop | FAppL of exp | FAppR of exp | FConsL of exp
  | FConsR of exp | FListRec of exp * exp | FLet of exp | FSeq of exp
  | FSend of proc * tp

  datatype status = SEval | SRetn | SSend of proc | SRecv of proc

  (* The memory for each process/machine. This will eventually change!
     TODO: turns out I don't think this needs to be part of the config, so
     maybe this should just be a type alias or something instead *)
  datatype mem = Mem of frame list

  (* Some type aliases for things so the config type can be slightly nicer *)

  datatype teval = MkEval of mem * exp
  fun unEval (MkEval x) = x

  datatype tretn = MkRetn of mem * exp
  fun unRetn (MkRetn x) = x

  (* Keep track of the process, type, and value here, since we know it *)
  datatype tsend = MkSend of mem * (proc * tp * exp)
  fun unSend (MkSend x) = x

  (* Keep track of the process and type here, since we know it *)
  datatype trecv = MkRecv of mem * (proc * tp)
  fun unRecv (MkRecv x) = x

  (* A + A + A + A = 4 * A *)

  datatype config = CEval of teval | CRetn of tretn | CSend of tsend
                  | CRecv of trecv

  fun ceval x = CEval (MkEval x)
  fun cretn x = CRetn (MkRetn x)
  fun csend x = CSend (MkSend x)
  fun crecv x = CRecv (MkRecv x)

  exception Stuck

  fun buildConfig s1 (CEval (MkEval (Mem s2, e2))) =
        ceval (Mem (s2 @ s1), e2)
    | buildConfig s1 (CRetn (MkRetn (Mem s2, e2))) =
        cretn (Mem (s2 @ s1), e2)
    | buildConfig s1 (CSend (MkSend (Mem s2, e2))) =
        csend (Mem (s2 @ s1), e2)
    | buildConfig s1 (CRecv (MkRecv (Mem s2, e2))) =
        crecv (Mem (s2 @ s1), e2)

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
    | ENil $ #[] => cretn (Mem [], e)
    | ECons $ #[e1, e2] => ceval (Mem [FConsL e2], e1)
    | EListRec $ #[el, en, ec] => ceval (Mem [FListRec (en, ec)], el)
    | ELet $ #[e1, e2] => ceval (Mem [FLet e2], e1)
    | ESeq $ #[e1, e2] => ceval (Mem [FSeq e2], e1)
    | ESend ptp $ #[e'] => ceval (Mem [FSend ptp], e')
    | ERecv ptp $ #[] => crecv (Mem [], ptp)


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
        (case Exp.map out (out e1) of
          ELam tp $ #[x \ b] => ceval (Mem s, subst e x b))
    | FConsL e2 => ceval (Mem (FConsR e :: s), e2)
    | FConsR e1 => cretn (Mem s, ECons $$ #[e1, e])
	  | FListRec (en, ec) =>
        (case out e of
          ENil $ #[] => ceval (Mem s, en)
        | ECons $ #[head, tail] =>
            (case map out (out ec) of
              x \ (y \ ec') =>
                ceval (Mem s, subst head x
                                (subst (EListRec $$ #[tail, en, ec]) y ec'))))
	  | FLet xe' =>
        (case out xe' of
          x \ e' => ceval (Mem s, subst e x e'))
	  | FSeq e2 => ceval (Mem s, e2)
	  | FSend (p, tp) => csend (Mem s, (p, tp, e))
  
  (* step : config -> config, only works if a step can be taken *)

  fun step (CEval (MkEval (Mem s, e))) = buildConfig s (eval e)
    | step (CRetn (MkRetn (Mem s, e))) = retn s e

  fun canStepAlone (CEval _) = true
    | canStepAlone (CRetn (MkRetn (Mem (_ :: _), _))) = true
    | canStepAlone _ = false

  fun runProgram e = if canStepAlone e then runProgram (step e) else e

  (*
    Now we need to write code that deals with the concurrency primitives, send
    and recv.
  *)

  datatype world =
    World of { running : (proc * time * config) list
             , sending : (proc * time * (mem * (proc * tp * exp))) list
             , recving : (proc * time * (mem * (proc * tp))) list
             }

  (*
    XXX Using records seems like the right thing to do here, but I'm not
    actually used to using them, so my style might be really bad here
  *)
    
  fun addToRunning (World {running = rs, sending = bs, recving = br}) r =
    World {running = r :: rs, sending = bs, recving = br }
  fun addToSending (World {running = rs, sending = bs, recving = br}) s =
    World {running = rs, sending = s :: bs, recving = br }
  fun addToRecving (World {running = rs, sending = bs, recving = br}) r =
    World {running = rs, sending = bs, recving = r :: br }

  fun modifyRunning (World {running = rs, sending = bs, recving = br}) r =
    World {running = r, sending = bs, recving = br }
  fun modifySending (World {running = rs, sending = bs, recving = br}) s =
    World {running = rs, sending = s, recving = br }
  fun modifyRecving (World {running = rs, sending = bs, recving = br}) r =
    World {running = rs, sending = bs, recving = r }

  fun partitionWorld (w as World { running = r, sending = bs, recving = br }) =
    case r of
      [] => w
    | (p, t, CSend cs) :: rs =>
        addToSending (partitionWorld (modifyRunning w rs)) (p, t, unSend cs)
    | (p, t, CRecv cs) :: rs =>
        addToRecving (partitionWorld (modifyRunning w rs)) (p, t, unRecv cs)
    | r :: rs => addToRunning (partitionWorld (modifyRunning w rs)) r

  fun findProcNum n [] = NONE
    | findProcNum n ((Proc i, t, e) :: rs) =
        if i = n then SOME e else findProcNum n rs

  (*
    Runs everything that can run for one step, then partitions out those
    that are now blocking.
  *)

  fun runRunners (w as World {running = rs, sending = bs, recving = br}) =
    partitionWorld
      (modifyRunning w
                     (List.map
                       (fn (p, t, c) =>
                         if canStepAlone c
                         then (p, tick t, step c)
                         else (p, t, c)) rs))

  (*
    findCompat p tp receivers finds a process in receivers that expects to
    receive a value of type tp from process p, or returns NONE if none is found.
  *)

  fun findCompat (psend : proc) (precv : proc) (tp : tp) [] = NONE
    | findCompat psend precv tp
                 ((r as (precvr, t, (m, (expSender, tp')))) :: rs) =
        if psend = expSender andalso precv = precvr
        then SOME (r, rs)
        else
          (case findCompat psend precv tp rs of
            SOME (x, rs') => SOME (x, r :: rs')
          | NONE => NONE)

  (* sync sees which blocking processes can communicate and has them do so *)

  fun sync (w as World {running = rs, sending = bs, recving = br}) =
    let
      fun inspectSenders [] receivers = ([], [], receivers)
        | inspectSenders ((sender as (psend, t, (s, (precv, tp, v))))::senders)
                         receivers =
              (case findCompat psend precv tp receivers of
                NONE =>
                  let
                    val (run, snd, rcv) = inspectSenders senders receivers
                  in
                    (run, sender :: snd, rcv)
                  end
              | SOME ((p', t', (s', (_, _))), receivers') =>
                  let
                    val (run, snd, rcv) = inspectSenders senders receivers'
                    (* XXX: send should return unit, not 0, but no unit yet *)
                    val newTime = Time.sync t t'
                    val runAfterSend = (psend, newTime,cretn (s, ENum 0 $$ #[]))
                    val runAfterRecv = (p', newTime, cretn (s', v))
                  in
                    (runAfterSend :: runAfterRecv :: run, snd, rcv)
                  end)
                  
      val (run, snd, rcv) = inspectSenders bs br
    in
      World { running = rs @ run, sending = snd, recving = rcv }
    end

  (* TODO: maybe also count it as terminating if there's a 'deadlock', but
     process 0 is done? or if 0 is done no matter what? Lots of options *)

  fun finishedWorld (World { running = rs, sending = [], recving = [] }) =
    not (List.exists (fn (_, _, x) => canStepAlone x) rs)
    | finishedWorld _ = false
    
  fun runWorld w =
    if finishedWorld w then w else runWorld (sync (runRunners w))

end
