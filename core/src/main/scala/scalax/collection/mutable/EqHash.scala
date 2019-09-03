package scalax.collection.mutable

import scala.collection.Util.nextPositivePowerOfTwo
import scala.collection.mutable.Growable

trait EqHash[A, This <: EqHash[A, This]] {
  this: IterableOnce[A] with Growable[A] with Equals =>

  protected def sizeHint: Int
  protected def step: Int

  protected var _size                  = 0
  final override def knownSize         = _size

  protected var (threshold: Int, table: Array[AnyRef]) = {
    val cap    = capacity(sizeHint)
    val length = len(cap)
    (length / 3, new Array[AnyRef](length))
  }

  def from(other: This) {
    threshold = other.threshold
    table = other.table.clone
    _size = other._size
  }

  @inline final protected def maxCapacity      = 1 << (31 - step)
  @inline final private def len(capacity: Int) = capacity * step
  @inline final private def cap(length: Int)   = length / step

  @inline final protected def nextKeyIndex(i: Int, len: Int) = {
    val n = i + step
    if (n < len) n else 0
  }

  import EqHash._

  @inline final protected def maskNull(key: AnyRef): AnyRef   = if (key eq null) maskedNull else key
  @inline final protected def unmaskNull(key: AnyRef): AnyRef = if (key eq maskedNull) maskedNull else key

  private def capacity(sizeHint: Int): Int = {
    val min = cap((len(1) + 1) * sizeHint)
    if (min < minCapacity) minCapacity
    else if (min > maxCapacity || min < 0) maxCapacity
    else nextPositivePowerOfTwo(min)
  }

  protected def index(maskedKey: AnyRef, keyHash: Int, tabLength: Int): Int = {
    val tab = table
    var i   = keyHash
    while (true) {
      val item = tab(i)
      if (item eq maskedKey)
        return i
      else if (item eq null)
        return ~i
      else
        i = nextKeyIndex(i, tabLength)
    }
    -1 // never reached
  }

  @inline final protected def index(key: AnyRef): Int = {
    val maskedKey = maskNull(key)
    val len       = table.length
    index(maskedKey, hash(maskedKey, len, step - 1), len)
  }

  override def clear() {
    java.util.Arrays.fill(table, null)
    _size = 0
  }

  protected def resize() {
    val oldTable  = table
    val oldLength = oldTable.length
    val newLength = 2 * oldLength
    if (oldLength == 2 * maxCapacity) {
      if (threshold == maxCapacity - 1)
        throw new IllegalStateException("Capacity exhausted.")
      threshold = maxCapacity - 1
    } else if (oldLength < newLength) {
      val newTable = new Array[AnyRef](newLength)
      threshold = newLength / 3
      move(oldTable, oldLength, newTable, newLength)
      table = newTable
    }
  }

  protected def move(oldTable: Array[AnyRef], oldLength: Int, newTable: Array[AnyRef], newLength: Int): Unit

  final protected def closeDeletion(index: Int) {
    // Knuth Section 6.4 Algorithm R
    val tab  = table
    val len  = tab.length
    val s    = step
    var d    = index
    var i    = nextKeyIndex(d, len)
    var item = tab(i)
    while (item ne null) {
      val r = hash(item, len, step - 1)
      if ((i < r && (r <= d || d <= i)) || (r <= d && d <= i)) {
        var j = 0
        while (j < s) {
          tab(d + j) = tab(i + j)
          tab(i + j) = null
          j += 1
        }
        d = i
      }
      i = nextKeyIndex(i, len)
      item = tab(i)
    }
  }

  abstract protected class EqHashIterator[A] extends Iterator[A] {
    protected val tab                   = table
    private[this] val len               = tab.length
    private[this] var index             = if (_size != 0) 0 else len
    private[this] var lastReturnedIndex = -1
    private[this] var indexValid        = false

    def hasNext: Boolean = {
      val s = step
      var i = index
      while (i < len) {
        if (tab(i) ne null) {
          index = i
          indexValid = true
          return true
        }
        i += s
      }
      index = len
      false
    }

    protected def nextIndex: Int = {
      if (!indexValid && !hasNext) throw new NoSuchElementException()

      indexValid = false
      lastReturnedIndex = index
      index += step
      return lastReturnedIndex
    }
  }

  protected class KeyIndexIterator extends EqHashIterator[Int] {
    def next: Int = nextIndex
  }

  override def hashCode: Int = {
    val tab = table
    new KeyIndexIterator().foldLeft(0)(_ + elemHashCode(tab, _))
  }

  def containsElem(elem: A): Boolean

  override def equals(other: Any): Boolean = other match {
    case that: EqHash[A, This] with IterableOnce[A] with Equals =>
      (that canEqual this) &&
        (that._size == this._size) &&
        (that.iterator.forall(containsElem))
    case _ => false
  }

  protected def elemHashCode: (Array[AnyRef], Int) => Int
}

object EqHash {

  final private[mutable] val defCapacity = 32
  final private[mutable] val minCapacity = 8

  final private[mutable] val maskedNull = new AnyRef

  @inline final private[this] def skipping(oHash: Int, len: Int, shift: Int): Int =
    // Multiply by -127 and left-shift to use least bit as part of hash
    ((oHash << shift) - (oHash << 8)) & (len - 1)

  @inline final private[this] def any(oHash: Int, len: Int): Int =
    (oHash - (oHash << 8)) & (len - 1)

  @inline final private[mutable] def hash(o: AnyRef, len: Int, shift: Int): Int = {
    val h = System.identityHashCode(o)
    if (shift == 0) any(h, len)
    else skipping(h, len, shift)
  }

  @inline final private[mutable] def evenHash(o: AnyRef, len: Int): Int =
    skipping(System.identityHashCode(o), len, 1)

  @inline final private[mutable] def anyHash(o: AnyRef, len: Int): Int =
    any(System.identityHashCode(o), len)

}
