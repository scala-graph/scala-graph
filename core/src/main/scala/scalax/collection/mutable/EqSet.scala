package scalax.collection.mutable

import scala.collection.Set
import scala.collection.JavaConversions._

import scalax.collection.immutable.{EqSet => ImmutableEqSet}

object EqSet {
  type EqSet[A<:AnyRef] = EqHashMap[A,Boolean]
  def apply[A<:AnyRef](size: Int) = new EqSet[A](size)
  
  /** Supplements or overrides `scala.collection.convert.JMapWrapperLike`.
   */
  implicit final class EqSetMethods[A<:AnyRef](val map: EqSet[A]) extends AnyVal {
    def apply(elem: A): Boolean = (map get (elem)) != null
    
    def add(elem: A): Boolean = (map put (elem, true)).isEmpty
    def += (elem: A): EqSet[A] = { add(elem); map }
    def ++=(elems: Traversable[A]): EqSet[A] = { elems foreach add; map }
    
    def remove(elem: A): Boolean = (map remove (elem)).isDefined
    def -= (elem: A): EqSet[A] = { remove(elem); map }
    def --=(elems: Traversable[A]): EqSet[A] = { elems foreach remove; map }
    
    def iterator: Iterator[A] = map.keysIterator
    def toKeySet: Set[A] = new ImmutableEqSet[A](map)
  }
}
object EqMap {
  type EqMap[K<:AnyRef,V] = EqHashMap[K,V]
  def apply[K<:AnyRef,V](size: Int) = new EqMap[K,V](size)

  /** Supplements or overrides `scala.collection.convert.JMapWrapperLike`.
   */
  implicit final class EqMapMethods[K<:AnyRef,V<:AnyRef]
      (val map: EqMap[K,V]) extends AnyVal {

    def toKeySet: Set[K] = new ImmutableEqSet[K](map)
  }
}