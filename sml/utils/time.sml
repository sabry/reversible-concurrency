structure Time :> TIME =
struct

  datatype t = Base of int * int | Comb of t * t * int * int
  (* Base is used when creating times, Comb is used when synchronizing *)

  val counter = ref 0 (* the base identifier for times *)

  fun access () =
    let
      val ref n = counter
      val () = counter := n + 1
    in
      n
    end

  fun newTime () = Base (access (), 0)

  fun sync t1 t2 =
    let
      val n = access ()
    in
      Comb (t1, t2, n, 0)
    end

  fun eq t1 t2 =
    case (t1, t2) of
      (Base (c1, n1), Base (c2, n2)) => c1 = c2 andalso n1 = n2
    | (Comb (tl1, tr1, c1, n1), Comb (tl2, tr2, c2, n2)) =>
        eq tl1 tl2 andalso eq tr1 tr2 andalso c1 = c2 andalso n1 = n2
    | _ => false

  fun leq t1 t2 =
    case (t1, t2) of
      (Base (c1, n1), Base (c2, n2)) => c1 = c2 andalso n1 <= n2

    | (Base (c1, n1), Comb (tl2, tr2, c2, n2)) => 
        leq t1 tl2 orelse leq t1 tr2

    | (Comb (tl1, tr1, c1, n1), Comb (tl2, tr2, c2, n2)) =>
        leq t1 tl2 orelse
        leq t1 tr2 orelse
        (c1 = c2 andalso n1 <= n2)

    | _ => false

  fun tick (Base (c, n)) = Base (c, n + 1)
    | tick (Comb (t1, t2, c, n)) = Comb (t1, t2, c, n + 1)

end
