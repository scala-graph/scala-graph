package scalax.collection
package mutable

import java.lang.Long.{bitCount, lowestOneBit => jLowestOneBit, toBinaryString}
import scala.annotation.tailrec
import scala.collection.mutable.BitSet

import State.Handle
import ExtBitSet.*

final protected[collection] class ExtBitSet(words: Array[Long]) extends BitSet(words) {
  def this(initWords: Int = incrWords) = this(new Array[Long](initWords))
  def nrWords: Int = nwords

  /** Array containing the internal words. */
  def cloneWords: Array[Long] = {
    val arr = new Array[Long](nrWords)
    Array.copy(elems, 0, arr, 0, nrWords)
    arr
  }
  override def clone(): ExtBitSet = new ExtBitSet(cloneWords)

  final override protected def className = getClass.getSimpleName

  /** All bits of all words. */
  override def toString =
    (elems map (w => "%64s".format(toBinaryString(w)).replace(' ', '0'))).mkString(" ")

  /** Summary of the words each of which formatted as <index>:<bitCount>. */
  def summary: String =
    elems.zipWithIndex.map(zWord => "%2d:%2d" format (zWord._2, bitCount(zWord._1))) mkString " "

  @inline def apply(idx: Int, mask: Long): Boolean =
    (word(idx) & mask) != 0

  @inline def apply(h: Handle): Boolean = apply(h.index, h.mask)

  def update(idx: Int, mask: Long, isSet: Boolean): Unit =
    if (isSet) {
      val word =
        if (idx >= nwords) { expand(idx); 0L }
        else elems(idx)
      elems(idx) = word | mask
    } else if (idx < nwords)
      elems(idx) = elems(idx) & ~mask

  @inline def update(h: Handle, isSet: Boolean): Unit = update(h.index, h.mask, isSet)

  def unary_~ : ExtBitSet = {
    val newBits = new ExtBitSet(nwords)
    var idx     = 0
    while (idx < nwords) {
      newBits.elems(idx) = ~this.elems(idx)
      idx += 1
    }
    newBits
  }

  def &=(other: ExtBitSet): this.type = {
    var idx = 0
    while (idx < nwords) {
      elems(idx) &= other.word(idx)
      idx += 1
    }
    this
  }

  def onOrFindUnset(other: ExtBitSet): Option[Handle] = {
    @tailrec def loop(idx: Int): Option[Handle] =
      if (idx < nwords) {
        val or = elems(idx) | other.word(idx)
        if (or == ~0) loop(idx + 1)
        else Some(new Handle(idx, jLowestOneBit(~or)))
      } else None

    loop(0)
  }

  def lowestOneBit: Option[Handle] = {
    @tailrec def loop(idx: Int): Option[Handle] =
      if (idx < nwords) {
        val bit = jLowestOneBit(elems(idx))
        if (bit == 0) loop(idx + 1)
        else Some(new Handle(idx, bit))
      } else None

    loop(0)
  }

  private def expand(mustHaveIdx: Int): Unit = {
    var newlen = nwords
    while (mustHaveIdx >= newlen) newlen = newlen + incrWords
    val newElems = new Array[Long](newlen)
    Array.copy(elems, 0, newElems, 0, nwords)
    elems = newElems
  }

  override protected def writeReplace(): ExtBitSet = this
}

object ExtBitSet {
  val incrWords = 8
}
