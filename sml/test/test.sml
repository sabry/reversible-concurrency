structure Test =
struct

  open Bexp;

  (*
  infix 6 +
  infix 7 *
  *)
  infix 6 \/;
  infix 7 /\;
  (*
  infix 5 <=
  infix 5 <
  *)


  infix 1 >>;
  (*
  infix 2 @
  *)

  open Exp;
  open ExpOps;
  open Conc;
  open ListUtils;

  type exp = Exp.t

  fun expToMachine e = ceval (Mem [], e)

  fun expsToWorld es =
        World { dict = ContDict.empty
              , running =
                  mapi (fn (e,i) =>
                         ((Proc i, Time.newTime (), []), expToMachine e)) es
              , sending = []
              , recving = []
              , backing = [] }

  fun extractFromConfig (CRetn (MkRetn (_, res))) = res

  (* XXX requires that process 0 exists and has finished! *)
  fun extractFromWorld (World { running = rs, ... }) =
    extractFromConfig (Option.valOf (findProcNum 0 rs))

  fun run e =
    let
      val res = runProgram (expToMachine e)
    in
      extractFromConfig res
    end

  fun runMulti es =
    let
      val res = runWorld (expsToWorld es)
    in
      extractFromWorld res
    end

  fun typecheckMulti es =
    let
      val checked as ((t, _) :: _) = List.map (typecheckExp Context.empty) es
    in
      (t
      ,foldr (fn ((_, cs), acc) => ContSet.union cs acc) ContSet.empty checked)
    end

  datatype result = Value of exp | Type of tp | ValAndType of exp * tp
                  (* | FailToTypecheck *)

  datatype test = Single of string * exp * result
                | Multi of string * (exp list) * result

  val tests : test list =
    [ Single ("3 = 3", n 3, Value (n 3))
    , Single ("3 + 4 = 7", (n 3) + (n 4), Value (n 7))
    , Single ("3 * 4 = 12", (n 3) * (n 4), Value (n 12))
    , Single ("if true then 3 else 4 ==> 3", eif (b true) (n 3) (n 4)
             , Value (n 3))
    , Single ("if false then 3 else 4 ==> 4", eif (b false) (n 3) (n 4)
             , Value (n 4))
    , Single ("true = true", b true
             , Value (b true))
    , Single ("true and false = false", (b true) /\ (b false)
             , Value (b false))
    , Single ("true and true = true", (b true) /\ (b true)
             , Value (b true))
    , Single ("true or false = true", (b true) \/ (b false), Value (b true))
    , Single ("true or true = true", (b true) \/ (b true), Value (b true))
    , Single ("false or false = false", (b false) \/ (b false), Value (b false))
    , Single ("Lx.x = Lx.x", lambda TInt (fn x => `` x),
                             Value (lambda TInt (fn x => `` x)))
    , Single ("(Lx.x) 3 = 3", lambda TInt (fn x => `` x) @ (n 3), Value (n 3))
    , Single ("nil = nil", emp TInt, Value (emp TInt))
    , Single ("[1,2,3] = [1,2,3]", cons (n 1) (cons (n 2) (cons (n 3) (emp TInt))),
                        Value (cons (n 1) (cons (n 2) (cons (n 3) (emp TInt)))))
    , Single ("fold + 0 [1,2,3] = 6",
       listrec (cons (n 1) (cons (n 2) (cons (n 3) (emp TInt))))
               (n 0)
               (fn (x, y) => `` x + `` y),
       Value (n 6))
    , Single ("let x = 3 in x + 4 = 7", elet (n 3) (fn x => `` x + (n 4))
             , Value (n 7))
    , Multi ("make sure multi works 1", [(n 3) + (n 4)], Value (n 7))
    , Multi ("make sure multi works 2", [(n 3) + (n 4), (n 0)], Value (n 7))
    , Multi ("receiving a value", 
             [recv (Proc 1) TInt, send (Proc 0) TInt (n 0)], Value (n 0))
    , Multi ("one thread backtracking",
             [elet (choose 0 (cons (n 0) (cons (n 4) (emp TInt))))
                   (fn x => eif ((`` x) <= (n 0)) (back 0) (`` x))],
             Value (n 4))
    , Multi ("two threads backtracking",
             [ elet (recv (Proc 1) TInt)
                    (fn x => eif ((`` x) <= (n 0)) (back 0) (`` x))
             , send (Proc 0) TInt (choose 0 (cons (n 0) (cons (n 4) (emp TInt))))
             ],
             Value (n 4))
    ]

  fun runTest (Single (s, ein, res)) =
    (case res of
      Value eout =>
      ((let
        val egot = run ein
      in
        (s, aequiv egot eout)
      end) handle _ => (s, false))
    | Type t =>
      ((let
        val (tgot, _) = typecheckExp Context.empty ein
      in
        (s, tgot = t)
      end) handle _ => (s, false))
    | ValAndType (eout, t) =>
      ((let
        val egot = run ein
        val (tgot, _) = typecheckExp Context.empty ein
      in
        (s, aequiv egot eout andalso tgot = t)
      end) handle _ => (s, false))
    )
    | runTest (Multi (s, es, res)) =
    (case res of
      Value eout =>
        ((let
          val egot = runMulti es
        in
          (s, aequiv egot eout)
        end) handle _ => (s, false))
    | Type t =>
        ((let
          val (tgot, _) = typecheckMulti es
        in
          (s, tgot = t)
        end) handle _ => (s, false))
    | ValAndType (eout, t) =>
        ((let
          val egot = runMulti es
          val (tgot, _) = typecheckMulti es
        in
          (s, aequiv egot eout andalso t = tgot)
        end) handle _ => (s, false)))

  fun doTest v (stuff, num) =
    let
      val (s, res) = runTest stuff
      val sOrNot = if v then s else ""
      val beginning = "Test #" ^ (Int.toString num) ^ ": " ^ sOrNot ^ "..."
      val rstring = if res then "Passed!" else "FAILED D:"
    in
      print (beginning ^ rstring ^ "\n")
    end

  fun testing v =
    let
      val _ = mapi (doTest v) tests
    in
      ()
    end

  val blarg = 
             [ elet (recv (Proc 1) TInt)
                    (fn x => eif ((`` x) <= (n 0)) (back 0) (`` x))
             , send (Proc 0) TInt (choose 0 (cons (n 0) (cons (n 4) (emp TInt))))
             ]

  val blerg = expsToWorld blarg

  fun runOneStep w = doBacktrack (sync (preempt (runRunners w)))
    
end
