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

object Util {

  /** A power of 2 >= `target`.
   */
  def powerOf2(target: Int): Int =
    scala.collection.mutable.HashTable.powerOfTwo(target)
}