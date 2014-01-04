package scalax.collection.immutable

import scala.collection.JavaConversions._

import scalax.collection.mutable.EqHashMap

/** Wrapper class mimicking a `scala.collection.immutable.Set`
 *  without copying the contents of the underlying `EqHashMap`.
 *  
 *  #define ON Creates a new `Set` as an O(N) operation
 */
final class EqSet[K <: AnyRef](map: EqHashMap[K,_]) extends Set[K] {
  def contains(key: K) = map containsKey(key)
  def iterator = map.keySet.iterator()
  /** $ON unless `elem` is already contained.*/
  def +(elem: K) = if (map contains elem) this else copy + elem  
  /** $ON unless `elem` is not contained.*/
  def -(elem: K) = if (map contains elem) copy - elem else this 
  private def copy: Set[K] = map.keysIterator.toSet
}