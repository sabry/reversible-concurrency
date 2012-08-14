signature CONTEXT =
sig

  structure Key : ORD_KEY
  type 'a context

  val empty : 'a context

  val insert : 'a context -> Key.ord_key -> 'a -> 'a context
  val remove : 'a context -> Key.ord_key -> 'a context * 'a

  val lookup : 'a context -> Key.ord_key -> 'a option

  val map : ('a -> 'b) -> 'a context -> 'b context
  val mapi : (Key.ord_key * 'a -> 'b) -> 'a context -> 'b context

end
