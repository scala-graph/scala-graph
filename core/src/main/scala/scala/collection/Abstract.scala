package scala.collection

object Util {

  /** A power of 2 >= `target`.
   */
  def powerOf2(target: Int): Int =
    scala.collection.mutable.HashTable.powerOfTwo(target)
}