structure ContKey =
struct

  open Base
  
  type ord_key = cont

  fun compare (Cont c1, Cont c2) = Int.compare (c1, c2)

end
