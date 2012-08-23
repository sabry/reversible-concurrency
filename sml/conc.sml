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
  | FSend of proc * tp | FChoose of cont

  datatype status = SEval | SRetn | SSend of proc | SRecv of proc

  fun out2 e = Exp.map out (out e)

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

  (* Make note that this process needs access to the time
     XXX this feels somewhat un-modular. The current placement of the time
     makes sense in a vacuum, but since this needs access to it, maybe it should
     go in the process' memory after all.
     However, this can be nicely cleaned up in the partitionWorld function,
     so maybe this is a good way of writing it after all. *)
  datatype ttime = MkTime of cont * mem * exp
  fun unTime (MkTime x) = x

  datatype tback = MkBack of mem * cont
  fun unBack (MkBack x) = x

  (* A + A + A + A = 4 * A *)

  datatype config = CEval of teval | CRetn of tretn | CSend of tsend
                  | CRecv of trecv | CTime of ttime | CBack of tback

  fun ceval x = CEval (MkEval x)
  fun cretn x = CRetn (MkRetn x)
  fun csend x = CSend (MkSend x)
  fun crecv x = CRecv (MkRecv x)
  fun ctime x = CTime (MkTime x)
  fun cback x = CBack (MkBack x)

  (* Utility functions for finding/removing processes in/from lists *)

  fun findProcByNum n [] = NONE
    | findProcByNum n ((x as ((Proc i, t, _), e)) :: rs) =
        if i = n then SOME x else findProcByNum n rs

  fun remProcByNum n [] = []
    | remProcByNum n ((x as ((Proc i, _, _), e)) :: rs) =
        if i = n then rs else remProcByNum n rs

  fun findProc (Proc p) ls = findProcByNum p ls

  fun remProc (Proc p) ls = remProcByNum p ls

  fun remJustProc p ls =
    List.map (fn (x, ()) => x) (remProc p (List.map (fn x => (x, ())) ls))

  fun findProcNum p ls =
    case findProcByNum p ls of
      NONE => NONE
    | SOME (_, e) => SOME e

  exception Stuck

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

  (*
    Now we need to write code that deals with the concurrency primitives, send
    and recv.
  *)

  datatype event = Sent of mem * (proc * tp * exp)
                 | Received of mem * (proc * tp)
                 | Chose of mem * cont * exp

  type timedEvent = time * event

  type extMem = proc * time * (timedEvent list)

  datatype world =
    World of { dict : (proc * time) ContDict.dict (* global information *)
             , running : (extMem * config) list
             , sending : (extMem * (mem * (proc * tp * exp))) list
             , recving : (extMem * (mem * (proc * tp))) list
             , backing : extMem list (* time refers to the time going back to *)
             }

  (*
    XXX Using records seems like the right thing to do here, but I'm not
    actually used to using them, so my style might be really bad here
  *)
    
  fun addToRunning
    (World {dict = d, running = rs, sending = bs, recving = br, backing = bt}) 
                   r =
    World
      { dict = d, running = r :: rs, sending = bs, recving = br, backing = bt }
  fun addToSending
    (World {dict = d, running = rs, sending = bs, recving = br, backing = bt}) 
                   s =
    World
      {dict = d, running = rs, sending = s :: bs, recving = br, backing = bt }
  fun addToRecving
    (World {dict = d, running = rs, sending = bs, recving = br, backing = bt}) 
                   r =
    World
      {dict = d, running = rs, sending = bs, recving = r :: br, backing = bt}
  fun addToBacking
    (World {dict = d, running = rs, sending = bs, recving = br, backing = bt}) 
                   b =
    World
      {dict = d, running = rs, sending = bs, recving = br, backing = b :: bt}

  fun modifyRunning
    (World {dict = d, running = rs, sending = bs, recving = br, backing = bt}) 
                    r =
    World {dict = d, running = r, sending = bs, recving = br, backing = bt}
  fun modifySending
    (World {dict = d, running = rs, sending = bs, recving = br, backing = bt}) 
                    s =
    World {dict = d, running = rs, sending = s, recving = br, backing = bt }
  fun modifyRecving
    (World {dict = d, running = rs, sending = bs, recving = br, backing = bt}) 
                    r =
    World {dict = d, running = rs, sending = bs, recving = r, backing = bt }
  fun modifyBacking
    (World {dict = d, running = rs, sending = bs, recving = br, backing = _}) 
                    b =
    World {dict = d, running = rs, sending = bs, recving = br, backing = b }
  fun modifyDict
    (World {dict = d, running = rs, sending = bs, recving = br, backing = bt}) 
                 d' =
    World {dict = d', running = rs, sending = bs, recving = br, backing = bt }











  fun appendToEnd v l =
    case out l of
      ENil _ $ #[] => ECons $$ #[v, l]
    | ECons $ #[v', l'] => ECons $$ #[v', appendToEnd v l']

  fun chooseAndShuffle e =
    case out e of
      ECons $ #[v, l] => (v, appendToEnd v l)



  (* added so I can keep partitionWorld relatively clean *)

  fun m1 f (x, y) z = (f x z, y)

  fun partitionWorld
        (w as World { dict = d, running = r, sending = bs, recving = br
                    , backing = bt }) =
    case r of
      [] => (w, [])
    | ((p, t, es), CSend cs) :: rs =>
        m1 addToSending (partitionWorld (modifyRunning w rs)) 
                        ((p, t, es), unSend cs)
    | ((p, t, es), CRecv cs) :: rs =>
        m1 addToRecving (partitionWorld (modifyRunning w rs))
                        ((p, t, es), unRecv cs)
    | ((p, t, es), CTime (MkTime (k, m, l))) :: rs =>
        let
          val (v, l') = chooseAndShuffle l
          val d' = ContDict.insert d k (p, t)
          val retnme = ((p, t, (t, Chose (m, k, l')) :: es), cretn (m, v))
        in
          m1 modifyDict
               (m1 addToRunning (partitionWorld (modifyRunning w rs)) retnme)
               d'
        end
    | ((p, t, es), CBack (MkBack (m, k))) :: rs =>
        let
          val SOME (p_back, t_back) =
            ContDict.lookup d k (* binding must exist! *)
          val retnme = ((p, t, es), cretn (m, ENum 0 $$ #[]))
          val (neww, backtracks) = partitionWorld (modifyRunning w rs)
          val beforeBacktracking = addToRunning neww retnme
        in
          (beforeBacktracking, (p_back, t_back) :: backtracks)
        end
    | r :: rs => m1 addToRunning (partitionWorld (modifyRunning w rs)) r

  (*
    Runs everything that can run for one step, then partitions out those
    that are now blocking.
  *)

  fun runRunners
    (w as World
      {dict = d, running = rs, sending = bs, recving = br, backing = bt}) =
    partitionWorld
      (modifyRunning w
                     (List.map
                       (fn ((p, t, es), c) =>
                         if canStepAlone c
                         then ((p, tick t, es), step c)
                         else ((p, t, es), c)) rs))

  (*
    findCompat p tp receivers finds a process in receivers that expects to
    receive a value of type tp from process p, or returns NONE if none is found.
  *)

  fun findCompat (psend : proc) (precv : proc) (tp : tp) [] = NONE
    | findCompat psend precv tp
                 ((r as ((precvr, t, erecv), (m, (expSender, tp')))) :: rs) =
        if psend = expSender andalso precv = precvr
        then SOME (r, rs)
        else
          (case findCompat psend precv tp rs of
            SOME (x, rs') => SOME (x, r :: rs')
          | NONE => NONE)

  (* sync sees which blocking processes can communicate and has them do so *)

  fun sync
    (w as World
      {dict = d, running = rs, sending = bs, recving = br, backing = bt}) =
    let
      fun inspectSenders [] receivers = ([], [], receivers)
        | inspectSenders ((sender as
                            ((psend, t, esend), (s, (precv, tp, v))))::senders)
                         receivers =
              (case findCompat psend precv tp receivers of
                NONE =>
                  let
                    val (run, snd, rcv) = inspectSenders senders receivers
                  in
                    (run, sender :: snd, rcv)
                  end
              | SOME (((p', t', erecv), (s', (_, _))), receivers') =>
                  let
                    val (run, snd, rcv) = inspectSenders senders receivers'
                    (* XXX: send should return unit, not 0, but no unit yet *)
                    val newTime = Time.sync t t'
                    val runAfterSend =
                      ((psend, newTime
                       , (newTime, Sent (s, (p', tp, v))) :: esend)
                      ,cretn (s, ENum 0 $$ #[]))
                    val runAfterRecv =
                      ((p', newTime
                       ,(newTime, Received (s', (psend, tp))) :: erecv)
                      ,cretn (s', v))
                  in
                    (runAfterSend :: runAfterRecv :: run, snd, rcv)
                  end)
                  
      val (run, snd, rcv) = inspectSenders bs br
    in
      World { dict = d, running = rs @ run, sending = snd, recving = rcv
            , backing = bt}
    end

  (* asyncBacktrack : extMem list ->
                        extMem list *
                        (extMem * config) list *
                        (extMem * (mem * (proc * tp * exp))) list * 
                        (extMem * (mem * (proc * tp))) list *
                        (proc * time) list *)

  fun asyncBacktrack bts =
    case bts of
      [] => ([], [], [], [], [])
      (* Always at least one event to backtrack towards *)
    | (p, t, []) :: bts => raise Stuck
    | (p, t, (tev, ev) :: evs) :: bts =>
        let
          val (backMore, toRun, toSend, toRecv, startBack) = asyncBacktrack bts
        in
          if Time.leq t tev
          then
            case ev of
             Sent (sinfo as (_, (ps, _, _))) =>
               (backMore, toRun, ((p, t, evs), sinfo) :: toSend,
                toRecv, (ps, t) :: startBack)
           | Received (rinfo as (_, (pr, _))) =>
               (backMore, toRun, toSend,
                ((p, t, evs), rinfo) :: toRecv, (pr, t) :: startBack)
           | Chose (Mem m, k, e) =>
               (backMore,
                ((p, t, evs), cretn (Mem (FChoose k :: m), e)) :: toRun, toSend,
                toRecv, startBack)
          else
            (case ev of
             Sent (_, (ps, _, _)) =>
               ((p, t, evs) :: backMore, toRun, toSend, toRecv,
                (ps, t) :: startBack)
           | Received (_, (pr, _)) =>
               ((p, t, evs) :: backMore, toRun, toSend, toRecv,
                (pr, t) :: startBack)
           | Chose (m, k, e) =>
               ((p, t, evs) :: backMore, toRun, toSend, toRecv, startBack))

        end

  (* startBacktrack : (proc * time) list -> (extMem * 'a) list ->
                      (extMem * 'a) list *
                      extMem list (???)
                      extMem list *)

  fun startBacktrack bs ps =
    case bs of
      [] => (ps, [], [])
    | (p, t) :: bs =>
      let
        val (forward, backward, leftover) = startBacktrack bs ps
      in
        case findProc p forward of
          NONE => (forward, backward, (p, t) :: leftover)
        | SOME ((_, _, evs), _) =>
            (remProc p forward, (p, t, evs) :: backward, leftover)
      end

  (* keepBacktracking : extMem list -> (extMem * unit) list -> extMem list *)
  (* XXX this type might be bad *)

  fun keepBacktracking' backMore startBack =                                  
    case backMore of
      [] => []                                                               
    | (x as (p, t, evs)) :: backMore =>
        (case findProc p startBack of                                        
          NONE => x :: keepBacktracking' backMore startBack
        | SOME ((_, t', evs'), ()) =>
            if Time.leq t t'
            then x :: keepBacktracking' backMore startBack                    
            else (p, t', evs) :: keepBacktracking' backMore startBack)

  fun keepBacktracking backMore startBack =
    keepBacktracking' backMore
    (* this is an awful hack *)
                      (List.map (fn (x,y) => ((x,y,[]), ())) startBack)

  fun doBacktrack
    (w as World
      { dict = d, running = rs, sending = bs, recving = br, backing = bts })
    =
    (*
      This is tricky.

      (1) Run all the backtracking one step. This will result in five things:
        - Processes that need to backtrack more
        - Processes that finished backtracking and now need to do something
        - A list of processes that need to start backtracking as a result
      
      (2) Preempt any processes that are currently running or blocking,
          including processes that just finished backtracking!

      (3) Make sure all processes that are currently backtracking are going back
          far enough.
    *)
    let
      val (backMore, toRun, toSend, toRecv, startBack) = asyncBacktrack bts
(*      val (rs', bts1, startBack) = startBacktrack startBack (rs @ toRun)
      val (bs', bts2, startBack) = startBacktrack startBack (bs @ toSend)
      val (br', bts3, startBack) = startBacktrack startBack (br @ toRecv)
      val backMore = keepBacktracking backMore startBack
    in
      World
        { dict = d, running = rs', sending = bs', recving = br'
        , backing = backMore @ bts1 @ bts2 @ bts3 } *)
    in
     (World
       { dict = d, running = toRun @ rs, sending = toSend @ bs
       , recving = toRecv @ br
       , backing = backMore }, startBack)
    end

  (* TODO: maybe also count it as terminating if there's a 'deadlock', but
     process 0 is done? or if 0 is done no matter what? Lots of options *)

  fun finishedWorld
    (World {dict = d, running = rs, sending = [], recving = [], backing = []}) =
      not (List.exists (fn (_, x) => canStepAlone x) rs)
    | finishedWorld _ = false

  fun backtrackProcess p t (wo as (World w)) =
    case findProc p (#running w) of
      SOME ((_, _, evs), _) =>
        modifyRunning (addToBacking wo (p, t, evs)) (remProc p (#running w))
    | NONE =>
      (case findProc p (#sending w) of
        SOME ((_, _, evs), _) =>
          modifySending (addToBacking wo (p, t, evs)) (remProc p (#sending w))
      | NONE =>
        (case findProc p (#recving w) of
          SOME ((_, _, evs), _) =>
            modifyRecving (addToBacking wo (p, t, evs)) (remProc p (#recving w))
        | NONE =>
          (case findProc p (List.map (fn x => (x, ())) (#backing w)) of
            SOME ((_, t', evs), ()) =>
              if Time.leq t' t
              then wo
              else
                addToBacking (modifyBacking wo (remJustProc p (#backing w))) 
                             (p, t, evs)
          | NONE => wo)))

  fun preempt (w, backtracks) =
    foldl (fn ((p, t), w) => backtrackProcess p t w) w backtracks
    
  fun runWorld w =
    if finishedWorld w
    then w
    else runWorld (preempt (doBacktrack (sync (preempt (runRunners w)))))


  (* Typechecking stuff below here *)


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
      ENum _ $ #[] => (TInt, ContSet.empty)
    | EBool _ $ #[] => (TInt, ContSet.empty)
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
