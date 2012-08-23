structure ContDict =
struct
  
  structure CD = BinaryMapFn (ContKey)

  type 'a dict = 'a CD.map

  val empty = CD.empty

  fun insert d k v = CD.insert (d, k, v)
  fun lookup d k = CD.find (d, k)

end
