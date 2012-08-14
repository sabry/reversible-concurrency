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
        World { running = mapi (fn (e,i) =>
                                 (Proc i, Time.newTime (), expToMachine e)) es
               , sending = [], recving = [] }

  fun extractFromConfig (CRetn (MkRetn (_, res))) = res

  (* XXX requires that process 0 exists and has finished! *)
  fun extractFromWorld (World { running = rs, sending = _, recving = _ }) =
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

  datatype test = Single of string * exp * exp
                | Multi of string * (exp list) * exp

  val tests : test list =
    [ Single ("3 = 3", n 3, n 3)
    , Single ("3 + 4 = 7", (n 3) + (n 4), n 7)
    , Single ("3 * 4 = 12", (n 3) * (n 4), n 12)
    , Single ("true = true", b true, b true)
    , Single ("true and false = false", (b true) /\ (b false), b false)
    , Single ("true and true = true", (b true) /\ (b true), b true)
    , Single ("true or false = true", (b true) \/ (b false), b true)
    , Single ("true or true = true", (b true) \/ (b true), b true)
    , Single ("false or false = false", (b false) \/ (b false), b false)
    , Single ("Lx.x = Lx.x", lambda TInt (fn x => `` x),
                             lambda TInt (fn x => `` x))  
    , Single ("(Lx.x) 3 = 3", lambda TInt (fn x => `` x) @ (n 3), n 3)
    , Single ("nil = nil", emp, emp)
    , Single ("[1,2,3] = [1,2,3]", cons (n 1) (cons (n 2) (cons (n 3) emp)),
                            cons (n 1) (cons (n 2) (cons (n 3) emp)))
    , Single ("fold + 0 [1,2,3] = 6",
       listrec (cons (n 1) (cons (n 2) (cons (n 3) emp)))
               (n 0)
               (fn (x, y) => `` x + `` y),
       n 6)
    , Single ("let x = 3 in x + 4 = 7", elet (n 3) (fn x => `` x + (n 4)), n 7)
    , Multi ("make sure multi works 1", [(n 3) + (n 4)], n 7)
    , Multi ("make sure multi works 2", [(n 3) + (n 4), (n 0)], n 7)
    , Multi ("receiving a value", 
             [recv (Proc 1) TInt, send (Proc 0) TInt (n 0)], n 0)
    ]

  fun runTest (Single (s, ein, eout)) =
    ((let
      val egot = run ein
    in
      (s, aequiv egot eout)
    end) handle _ => (s, false))
    | runTest (Multi (s, es, eout)) =
    (let
      val egot = runMulti es
    in
      (s, aequiv egot eout)
    end) handle _ => (s, false)

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
    
end
