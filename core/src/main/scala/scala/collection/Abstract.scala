package scala.collection

/** Increase access to some `private[scala]` members of `scala.collection`
 *  to make them reusable in any package. 
 */
object Abstract {
  abstract class Traversable[A] extends scala.collection.AbstractTraversable[A]
  abstract class Iterable[A] extends scala.collection.AbstractIterable[A]
  abstract class Iterator[A] extends scala.collection.AbstractIterator[A]
}

/** Wraps `t` to a `Seq`. It helps to avoid the creation of a copy of the elements of `t`
 *  when passing `t` to repeated parameters of type `A`. `apply` is O(N).
 *     
 * @param t the underlying `Traversable`.
 */
class SeqFacade[A](t: Traversable[A]) extends Seq[A] {
   def iterator: Iterator[A] = t.toIterator
   def apply(idx: Int): A = {
     val it = iterator
     var i = 0
     while (i < idx) { it.next; i += 1 }
     it.next
   }
   def length: Int = t.size
}