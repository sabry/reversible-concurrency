structure TopUtils =
struct

  datatype ('a, 'b) sum = Inl of 'a | Inr of 'b

  fun id x = x
  fun swap (x, y) = (y, x)

  fun curry f x y = f (x, y)
  fun uncurry f (x, y) = f x y
  
  fun fst (x, _) = x
  fun snd (_, y) = y
  fun pairmap (f, g) (x, y) = (f x, g y)
  fun pairmap1 f (x, y) = (f x, y)
  fun pairmap2 g (x, y) = (x, g y)

end
