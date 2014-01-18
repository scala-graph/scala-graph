package scalax.collection.mutable

import scala.collection.Set
import scala.collection.JavaConversions._

import scalax.collection.immutable.{EqSet => ImmutableEqSet}

object EqSet {
  type EqSet[A<:AnyRef] = EqHashMap[A,Boolean]
  def apply[A<:AnyRef](size: Int) = new EqSet[A](size)
  
  /** Supplements or overrides `scala.collection.convert.JMapWrapperLike`.
   */
  implicit class EqSetMethods[A<:AnyRef](val map: EqSet[A]) extends AnyVal {
    def apply(elem: A): Boolean = (map get (elem)) != null
    def += (elem: A): map.type = { map put (elem, true); map }
    def iterator: Iterator[A] = map.keysIterator
    def toKeySet: Set[A] = new ImmutableEqSet[A](map)
  }
}
object EqMap {
  type EqMap[K<:AnyRef,V] = EqHashMap[K,V]
  def apply[K<:AnyRef,V](size: Int) = new EqMap[K,V](size)

  /** Supplements or overrides `scala.collection.convert.JMapWrapperLike`.
   */
  implicit class EqMapMethods[K<:AnyRef,V<:AnyRef]
      (val map: EqMap[K,V]) extends AnyVal {

    def toKeySet: Set[K] = new ImmutableEqSet[K](map)
  }
}