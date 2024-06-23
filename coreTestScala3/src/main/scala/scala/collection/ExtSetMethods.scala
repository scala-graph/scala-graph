package scala.collection

import scala.util.Random

trait ExtSetMethods[A] extends FilterableSet[A] {
  this: Set[A] =>

  /** Returns a random element of the set if it is `nonEmpty` otherwise throws `IllegalArgumentException`.
    * Note that currently a near- but no true-uniform distribution is granted to allow for O(1) implementation.
    *
    *  @param random a random generator; it is essential that `random` be instantiated
    *                by the caller just once for any sequence of calls
    */
  def draw(random: Random): A

  def findElem(elem: A): Option[A] = Option(findElem[A](elem, _ == _))

  /** Finds an entry in the collection based on `toMatch`'s `hashCode` and a correspondence
    * function but not on the argument type.
    *
    *  @param toMatch a value not necessarily of type `A`; the entry to be searched for
    *               must have the same `hashCode` and be equal to this argument
    *  @param correspond function returning whether  a given entry corresponds to `other`
    *  @return the entry corresponding to `toMatch` or null if not contained
    */
  def findElem[B](toMatch: B, correspond: (A, B) => Boolean): A
}
