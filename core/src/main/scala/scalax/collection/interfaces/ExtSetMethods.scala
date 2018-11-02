package scalax.collection
package interfaces

import scala.util.Random

import collection.FilteredSet

trait ExtSetMethods[A] {
  this: AnySet[A] =>

  /** Returns a random element of the set if it is `nonEmpty` otherwise throws
    *  `IllegalArgumentException`.
    *
    *  @param random a random generator; it is essential that `random` be instantiated
    *                by the caller just once for any sequence of calls
    */
  def draw(random: Random): A

  /** Finds an entry in the collection based on `toMatch`'s `hashCode` and a correspondence
    *  function but not on the argument type.
    *
    *  @param toMatch a value not necessarily of type `A`; the entry to be searched for
    *               must have the same `hashCode` and be equal to this argument
    *  @param correspond function returning whether  a given entry corresponds to `other`
    *  @return the entry corresponding to `toMatch` or null if not contained
    */
  def findElem[B](toMatch: B, correspond: (A, B) => Boolean): A

  /** Returns a lazily filtered immutable Set. */
  def withSetFilter(p: (A) => Boolean): FilteredSet[A] = new FilteredSet(this, p)
}
