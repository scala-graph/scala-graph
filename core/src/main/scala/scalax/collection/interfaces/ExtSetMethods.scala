package scalax.collection.interfaces

import util.Random

trait ExtSetMethods[A] {
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
  def findEntry[B](toMatch: B, correspond: (A, B) => Boolean): A
}