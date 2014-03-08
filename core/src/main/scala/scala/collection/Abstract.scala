package scala.collection

/** Contains types that increase access to some `private[scala]` members of
 *  `scala.collection` in order to make them reusable in any package. 
 */
object Abstract {
  type Traversable[A] = scala.collection.AbstractTraversable[A]
  type Iterable[A]    = scala.collection.AbstractIterable[A]
  type Iterator[A]    = scala.collection.AbstractIterator[A]
  type Set[A]         = scala.collection.AbstractSet[A]
}

/** Wraps `t` to a [[scala.collection.Seq Seq]]. It helps to avoid the creation of a copy
 *  of the elements of `t` when passing `t` to repeated parameters of type `A`.
 *  `apply` is O(N).
 *     
 * @param t the underlying `Traversable`.
 */
final class SeqFacade[A](t: Traversable[A]) extends Seq[A] {
   def iterator: Iterator[A] = t.toIterator
   def apply(idx: Int): A = {
     val it = iterator
     var i = 0
     while (i < idx) { it.next; i += 1 }
     it.next
   }
   def length: Int = t.size
}

/** Wraps the [[scala.collection.Traversable Traversable]] `t` to a
 *  [[scala.collection.immutable.Set Set]].
 *  It aims at efficiently creating a set in case the caller ensures that
 *  all elements in `t` are unique.
 *  `+` and `-` are O(N) returning [[scala.collection.immutable.Set]].  
 *     
 * @param t the underlying `Traversable` with unique elements.
 */
class SetFacade[A](t: Traversable[A]) extends immutable.Set[A] {
   final def iterator: Iterator[A] = t.toIterator
   def contains(elem: A) = t exists (_ == elem)
   def -(elem: A) = t.toSet - elem 
   def +(elem: A) = t.toSet + elem
   override final def size: Int = t.size
}

/** Wraps the [[scala.collection.Traversable Traversable]] `t` to a
 *  [[scala.collection.immutable.Set Set]] utilizing reference equality.
 *  It aims at efficiently creating a set in case the caller ensures that
 *  all elements in `t` are unique.
 *  `+` and `-` are O(N) returning [[scala.collection.immutable.Set]].  
 *     
 * @param t the underlying `Traversable` with unique elements.
 */
final class EqSetFacade[A <: AnyRef](t: Traversable[A]) extends SetFacade[A](t) {
   final override def contains(elem: A) = t exists (_ eq elem)
}