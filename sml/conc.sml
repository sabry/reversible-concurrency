(*
  This file is long, but there's a lot of boilerplate and long function defs,
  thanks mostly to my use of records here.
*)

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

  open SingleThreadedEvaluator

  (*
    Now we need to write code that deals with the concurrency primitives, send
    and recv.
  *)

  datatype event = Sent of mem * (proc * tp * exp)
                 | Received of mem * (proc * (proc list) * tp)
                 | Chose of mem * cont * exp

  type timedEvent = time * event

  type extMem = proc * time * (timedEvent list)


  (*
    The record makes it easy to tell which field of what would otherwise be a
    tuple is doing what, but at the cost of making a ton of very long lines of
    code (for example, the functions defined below that modify the world record.
    I'm not 100% convinced that this is a worthwhile tradeoff, but I'm even less
    convinced that it would be a good use of time to change it back to tuples at
    this point.
  *)

  datatype world =
    World of { dict : (proc * time) ContDict.dict (* global information *)
             , running : (extMem * config) list
             , sending : (extMem * (mem * (proc * tp * exp))) list
             , recving : (extMem * (mem * (proc list * tp))) list
             , backing : extMem list (* time refers to the time going back to *)
             }

    
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


  (*
    Appends to the end of an object-language list.
  *)

  fun appendToEnd v l =
    case out l of
      ENil _ $ #[] => ECons $$ #[v, l]
    | ECons $ #[v', l'] => ECons $$ #[v', appendToEnd v l']

  (*
    Chooses a value from the list and "shuffles" it.

    This is currently implemented by taking the first element, then moving it to
    the end. The redex machine semantics do not remove choices, so neither does
    this implementation; moving it to the end is to avoid repeats.

    Another valid implementation would be randomly selecting an element.
    However, after that, we would need to decide whether we wanted to keep the
    list the same (replacement) or remove it from the list into a temporary
    "already chosen" list (without replacement) until everything in the original
    list has been chosen once.
  *)

  fun chooseAndShuffle e =
    case out e of
      ECons $ #[v, l] => (v, appendToEnd v l)


  (*
    added so I can keep partitionWorld relatively clean. Applies the function
    and an argument to only the first element of a tuple.
    
  *)

  fun m1 f (x, y) z = (f x z, y)

  (*
    partitionWorld runs after everything in the runqueue has been stepped. At
    first, the runqueue contains only expressions that are evaluating or
    returning, but after step, they could be in any state. This code takes them
    out of the runqueue and into the appropriate location in the code.

    XXX: The order in which things are run means that some expressions get to
    run more than one step per "global" step. For example, if something is
    backtracking, it is put into the backtracking list, then gets backtracked
    as the next step of global evaluation. This seems potentially undesirable,
    but I have left it as-is for now in the interest of simplicity.
  *)

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
                 ((r as ((precvr, t, erecv), (m, (expSenders, tp')))) :: rs) =
        if precv = precvr andalso List.exists (fn x => x = psend) expSenders
                          andalso tp = tp'
        then SOME (r, rs, expSenders)
        else
          (case findCompat psend precv tp rs of
            SOME (x, rs', es) => SOME (x, r :: rs', es)
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
              | SOME (((p', t', erecv), (s', (_, _))), receivers', rs) =>
                  let
                    val (run, snd, rcv) = inspectSenders senders receivers'
                    (* TODO: send should return unit, not 0, but no unit yet *)
                    val newTime = Time.sync t t'
                    val runAfterSend =
                      ((psend, newTime
                       , (newTime, Sent (s, (p', tp, v))) :: esend)
                      ,cretn (s, ENum 0 $$ #[]))
                    val runAfterRecv =
                      ((p', newTime
                       ,(newTime, Received (s', (psend, rs, tp))) :: erecv)
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
                        (extMem * (mem * (proc * proc list * tp))) list *
                        (proc * time) list *)
  
  (*
    asyncBacktrack is a busy function. Its high-level goal is to run everything
    that is currently backtracking through one step of backtracking. That is,
    each process 'remembers' every choice it has made and every time it has
    synchronized and keeps that information as a stack; asyncBacktrack moves the
    process one level up the stack.

    This means that backtracking processes could end up in any state,
    depending on where they needed to backtrack to. Additionally, they can cause
    other processes to backtrack. Thus, the return value is a tuple of the
    various things a backtracking process can become, as well as a list of other
    processes that need to backtrack and times they need to backtrack to.

    TODO: When writing this commend, it occurred to me that this could probably
    be done by returning one list of states, then running partitionWorld again.
    Would still need to preempt other processes, but it would make everything
    much, much simpler.
  *)

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
           | Received (rinfo as (_, (pr, _, _))) =>
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
           | Received (_, (pr, _, _)) =>
               ((p, t, evs) :: backMore, toRun, toSend, toRecv,
                (pr, t) :: startBack)
           | Chose (m, k, e) =>
               ((p, t, evs) :: backMore, toRun, toSend, toRecv, startBack))

        end

  (*
    asyncBacktrack has extra information in the received stuff. This gets rid
    of it.
  *)

  fun cleanReceivedMetadata (e, (m, (p, ps, t))) = (e, (m, (ps, t)))

  (*
    doBacktrack runs asyncBacktrack, then updates the world's state.
  *)

  fun doBacktrack
    (w as World
      { dict = d, running = rs, sending = bs, recving = br, backing = bts })
    =
    let
      val (backMore, toRun, toSend, toRecv, startBack) = asyncBacktrack bts
      val toRecv = List.map cleanReceivedMetadata toRecv
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

  (*
    backtrackProcess backtracks a given process in the world. If it is not
    already backtracking, it will be preempted and told to backtrack. If it is
    already backtracking, it will start to backtrack to the earlier of the two
    times.
  *)

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

end
