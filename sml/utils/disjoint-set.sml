functor DisjointSetFn (Ord : ORD) : DISJOINT_SET =
struct
  
  structure O = Ord

  exception Repeat of O.t

  type t = O.t list

  val empty = []

  fun eq t1 t2 =
    case O.compare t1 t2 of
      EQUAL => true
    | _ => false

  fun mem x ls =
    case ls of
      [] => false
    | y :: ls => if eq x y then true else mem x ls

  fun insertInOrderNoRepeats x ls =
    case ls of
      [] => SOME [x]
    | y :: ls =>
        (case O.compare x y of
          EQUAL => NONE
        | LESS => SOME (x :: y :: ls)
        | GREATER =>
            Option.map (fn ls' => y :: ls') (insertInOrderNoRepeats x ls))

  fun insert s t =
    case insertInOrderNoRepeats t s of
      NONE => raise Repeat t
    | SOME x => x

  fun singleton x = [x]

  fun union l1 l2 =
    foldr (fn (elem, acc) => insert acc elem) l2 l1

end
