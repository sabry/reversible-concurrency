structure ListUtils =
struct
  fun mapi f l =
    List.map f (ListPair.zip (l, List.tabulate (List.length l, fn x => x)))

  (* Must be the same length *)
  fun map2 f l1 l2 =
    case (l1, l2) of
      ([], []) => []
    | (x :: xs, y :: ys) => (f x y) :: (map2 f xs ys)
end
