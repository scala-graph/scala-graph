package scalax.collection.immutable

import scalax.collection.mutable.EqHashMap

/** Wrapper class mimicking a `scala.collection.immutable.Set`
  *  without copying the contents of the underlying `EqHashMap`.
  *
  *  @define ON Creates a new `Set` as an O(N) operation
  */
final class EqSet[K <: AnyRef](map: EqHashMap[K, _]) extends Set[K] {

  def contains(key: K) = map contains key
  def iterator         = map.keysIterator

  /** $ON unless `elem` is already contained.*/
  def incl(elem: K) =
    if (map contains elem) this
    else {
      val newMap = map.clone
      newMap.asInstanceOf[EqHashMap[K, Any]] put (elem, true)
      new EqSet(newMap)
    }

  /** $ON unless `elem` is not contained.*/
  def excl(elem: K) =
    if (map contains elem) {
      val newMap = map.clone
      newMap -= elem
      new EqSet(newMap)
    } else this

  override def size = map.size
}
