structure EvalUtils =
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

  (*
    The abstract machine for evaluation consists of frames that are pushed onto
    an evaluation stack. There is a frame for each location in the code where
    an expression needs to be evaluated. Frames are stored in a list that acts
    as a stack.
  *)

  datatype frame =
    FBinopL of binop * exp | FIf of exp * exp | FBinopR of binop * exp
  | FUnop of unop | FAppL of exp | FAppR of exp | FConsL of exp
  | FConsR of exp | FListRec of exp * exp | FLet of exp | FSeq of exp
  | FSend of proc * tp | FChoose of cont

  fun out2 e = Exp.map out (out e)

  (*
    The local memory for each process/machine.
    XXX: This might just need to be a type alias or something
  *)

  datatype mem = Mem of frame list

  (* Some "newtypes" for things so the config type can be slightly nicer *)

  (*
    When evaluating and returning, we only need the stack and the expression
    being evaluated or returned.
  *)

  datatype teval = MkEval of mem * exp
  fun unEval (MkEval x) = x

  datatype tretn = MkRetn of mem * exp
  fun unRetn (MkRetn x) = x

  (*
    tsend is used to keep track of processes that are waiting to send. We need
    to know the state the process is ready to return to once the send occurs,
    the process it wants to send to, the type of the value being send, and the
    actual value being sent.

    INVARIANT: The expression must be a value.
  *)

  datatype tsend = MkSend of mem * (proc * tp * exp)
  fun unSend (MkSend x) = x

  (*
    trecv is used to keep track of processes that are waiting to receive. The
    information is similar to the sending information, except we keep a list of
    processes in order to allow nondeterminism in the code.
  *)

  datatype trecv = MkRecv of mem * (proc list * tp)
  fun unRecv (MkRecv x) = x

  (*
    Make note that this process needs access to the time. Currently used only
    for choices.

    XXX: This feels somewhat un-modular. The current placement of the time
    makes sense in a vacuum, but since this needs access to it, maybe it should
    go in the process' memory after all.

    However, this can be nicely cleaned up in the partitionWorld function,
    so maybe this is a good way of writing it after all.
  *)

  datatype ttime = MkTime of cont * mem * exp
  fun unTime (MkTime x) = x

  (*
    tback tells the runtime to start backtracking the process denoted by cont.
    
    NB: This will not necessarily backtrack the process calling it, so it is
    necessary to keep track of the stack in case, for example, process 0 is
    telling process 1 to backtrack, so process 0 can continue on its way.
  *)

  datatype tback = MkBack of mem * cont
  fun unBack (MkBack x) = x

  (* A valid configuration can be any of the above *)

  datatype config = CEval of teval | CRetn of tretn | CSend of tsend
                  | CRecv of trecv | CTime of ttime | CBack of tback

  (* Some handy wrapper functions to avoid excessive syntax *)

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

end
