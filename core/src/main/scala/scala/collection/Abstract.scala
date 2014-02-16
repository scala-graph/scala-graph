package scala.collection

/** Increase access to some `private[scala]` members of `scala.collection`
 *  to make them reusable in any package. 
 */
object Abstract {
  abstract class Traversable[A] extends scala.collection.AbstractTraversable[A]
  abstract class Iterable[A] extends scala.collection.AbstractIterable[A]
  abstract class Iterator[A] extends scala.collection.AbstractIterator[A]
}