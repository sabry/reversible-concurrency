structure Var :> VAR =
struct

  type t = string * int

  val counter = ref 0

  fun newvar () =
    let
      val (ref n) = counter
    in
      (counter := n + 1 ; ("", n))
    end

  fun named s =
    let
      val (ref n) = counter
    in
      (counter := n + 1 ; (s, n))
    end

  fun eq (_, n) (_, m) = (n = m)

  fun compare ((_, n), (_, m)) = Int.compare (n, m)

  fun toString (s, x) =
    (case s of
      "" => "anon@"
    | _ => s)
    ^ (Int.toString x)

end
