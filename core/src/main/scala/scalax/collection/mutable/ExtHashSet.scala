package scalax.collection
package mutable

import scala.util.Random
import scala.collection.AbstractIterator
import scala.collection.mutable.{GrowingBuilder, HashSet, Set, SetLike}
import scala.collection.generic.{GenericSetTemplate, MutableSetFactory}

import interfaces.ExtSetMethods

/* Alternatively we could use `hashTableContents` as suggested by Rüdiger Klaehn like

package scala.collection.mutable
object HashSetExt {
  implicit class HashSetDraw[A](val value: HashSet[A]) extends AnyVal {
    def draw(random:Random) : A = {
      val table = value.hashTableContents.table
      ...
 */
class ExtHashSet[A]
    extends HashSet[A]
    with SetLike[A, ExtHashSet[A]]
    with GenericSetTemplate[A, ExtHashSet]
    with ExtSetMethods[A] {
  def empty[A]: Set[A]   = new ExtHashSet[A]
  override def companion = ExtHashSet

  def draw(random: Random): A = {
    val len   = table.length
    val drawn = random.nextInt(len) // IllegalArgumentException if len == 0
    def search(from: Int, incr: Int, hold: Int => Boolean): Option[A] = {
      var i = from
      while (hold(i)) {
        val elem = table(i)
        if (elem ne null) return Some(elem.asInstanceOf[A])
        else i += incr
      }
      None
    }
    // must always find an element either upwards or downwards
    search(drawn, 1, (i: Int) => i < len) getOrElse (search(drawn - 1, -1, (i: Int) => i > 0).get)
  }

  def findElem(elem: A): Option[A] = findEntry(elem)

  def findElem[B](other: B, correspond: (A, B) => Boolean): A = {
    var entry: AnyRef = null
    if (null != other) {
      var h = index(other.hashCode)
      entry = table(h)
      while (null != entry && !correspond(entry.asInstanceOf[A], other)) {
        h = (h + 1) % table.length
        entry = table(h)
      }
    }
    entry.asInstanceOf[A]
  }

  /** Returns an `Iterator` over all entries having the passed `hashCode`.
    */
  def hashCodeIterator(hcode: Int): Iterator[A] = new AbstractIterator[A] {
    private var h     = index(hcode)
    private var entry = table(h)

    def hasNext = null != entry && entry.hashCode == hcode
    def next = {
      val prevEntry = entry
      h = (h + 1) % table.length
      entry = table(h)
      prevEntry.asInstanceOf[A]
    }
  }

  /** Updates or inserts `elem`.
    *  @return `true` if an update took place.
    */
  protected[collection] def upsert(elem: A with AnyRef): Boolean = {
    var h     = index(elem.##)
    var entry = table(h)
    while (null != entry) {
      if (entry == elem) {
        table(h) = elem
        return false
      }
      h = (h + 1) % table.length
      entry = table(h)
    }
    addEntry(elem)
  }
}

object ExtHashSet extends MutableSetFactory[ExtHashSet] {
  override def empty[A]      = new ExtHashSet[A]
  override def newBuilder[A] = new GrowingBuilder[A, ExtHashSet[A]](empty[A])
}
