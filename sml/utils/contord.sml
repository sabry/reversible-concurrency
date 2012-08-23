structure ContOrd =
struct

  open Base
  
  type t = cont

  fun compare (Cont c1) (Cont c2) = Int.compare (c1, c2)

end
