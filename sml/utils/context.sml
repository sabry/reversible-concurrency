structure Context :> CONTEXT where type Key.ord_key = Var.t =
struct
  structure M =
    BinaryMapFn
      (struct type ord_key = Var.t val compare = Var.compare end)

  type 'a context = 'a M.map
  structure K = M.Key
  structure Key = K

  val empty = M.empty

  fun insert ctx k v = M.insert (ctx, k, v)
  fun remove ctx k = M.remove (ctx, k)

  fun lookup ctx k = M.find (ctx, k)

  val map = M.map
  val mapi = M.mapi
end
