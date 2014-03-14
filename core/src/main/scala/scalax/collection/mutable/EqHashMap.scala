package scalax.collection.mutable

import scala.collection.mutable.{Map, MapLike}
import scala.language.implicitConversions

import scala.collection.Util.powerOf2

class EqHashMap[K <: AnyRef, V](sizeHint: Int = EqHashMap.defCapacity)
    extends Map[K,V]
    with    MapLike[K,V,EqHashMap[K,V]] {

  import EqHashMap.{defCapacity, minCapacity, maxCapacity, capacity, hash, nextKeyIndex}
  
  @transient private var (threshold: Int, table: Array[AnyRef]) = {
    val cap = capacity(sizeHint)
    val length = len(cap)
    (length / 3, new Array[AnyRef](length))
  }
  
  private var _size = 0
  @inline final override def size = _size
  
  def this(other: EqHashMap[K,V]) {
    this()
    threshold = other.threshold
    table = other.table.clone
    _size = other.size
  }

  @inline final private def emptyKey: K = EqHashMap._emptyKey
  @inline final private def   maskNull(key: K): K = if (key eq null) emptyKey else key
  @inline final private def unmaskNull(key: K): K = if (key eq emptyKey) emptyKey else key

  @inline final private implicit def toK(o: AnyRef): K = o.asInstanceOf[K]
  @inline final private implicit def toV(o: AnyRef): V = o.asInstanceOf[V]

  override def empty: EqHashMap[K,V] = EqHashMap.empty[K,V](defCapacity)

  def getOrNull(key: K): AnyRef = {
    val k = maskNull(key)
    val len = table.length
    var i = hash(k, len)
    while (true) {
      val item = table(i)
      if (item eq k)
        return table(i + 1)
      if (item eq null)
        return null
      i = nextKeyIndex(i, len)
    }
    null
  }

  @inline final override def apply(key: K): V =
  {
    val value = getOrNull(key)
    if (value eq null) default(key)
    else value
  }

  @inline final def get(key: K): Option[V] = Option(getOrNull(key))

  @inline final override def contains(key: K): Boolean = getOrNull(key) ne null
  
  private def closeDeletion(index: Int) { // Knuth Section 6.4 Algorithm R
    val len = table.length
    var d = index
    var i = nextKeyIndex(d, len)
    var item = table(i)
    while (item ne null) {
      val r = hash(item, len)
      if ((i < r && (r <= d || d <= i)) || (r <= d && d <= i)) {
        table(d) = item;
        table(d + 1) = table(i + 1);
        table(i) = null;
        table(i + 1) = null;
        d = i;
      }
      i = nextKeyIndex(i, len)
      item = table(i)
    }
  }

  override def remove(key: K): Option[V] = {
    val k = maskNull(key)
    val len = table.length
    var i = hash(k, len)
    while (true) {
      val item = table(i)
      if (item eq k) {
        _size -= 1
        val oldValue = table(i + 1)
        table(i + 1) = null
        table(i) = null
        closeDeletion(i)
        return Some(oldValue)
      }
      if (item eq null)
        return None
      i = nextKeyIndex(i, len)
    }
    None
  } 
  
  def -=(key: K) = { remove(key); this }
  
  override def clear {
    java.util.Arrays.fill(table, null)
    _size = 0
  }

  @inline final private def len(capacity: Int) = capacity * 2
  @inline final private def cap(length: Int) = length / 2
  
  private def resize(newCapacity: Int) {
    val newLength = len(newCapacity)
    val oldTable = table
    val oldLength = oldTable.length
    if (oldLength == 2 * maxCapacity) {
      if (threshold == maxCapacity - 1)
        throw new IllegalStateException("Capacity exhausted.")
      threshold = maxCapacity - 1
    } else if (oldLength < newLength) {
      val newTable = new Array[AnyRef](newLength)
      threshold = newLength / 3

      var j = 0
      while (j < oldLength) {
        val key = oldTable(j);
        if (key != null) {
          val value = oldTable(j + 1)
          oldTable(j) = null
          oldTable(j + 1) = null
          var i = hash(key, newLength)
          while (newTable(i) ne null)
            i = nextKeyIndex(i, newLength)
          newTable(i) = key
          newTable(i + 1) = value
        }
        j += 2
      }
      table = newTable    
    }
  }
  
  override def put(key: K, value: V): Option[V] = {
    val k = maskNull(key)
    val tab = table
    val len = tab.length
    var i = hash(k, len)
    var item = tab(i)
    while (item ne null) {
      if (item eq k) {
        val oldValue = tab(i + 1)
        table(i + 1) = value.asInstanceOf[AnyRef]
        return Some(oldValue)
      }
      i = nextKeyIndex(i, len);
      item = tab(i)
    }
    tab(i) = k
    tab(i + 1) = value.asInstanceOf[AnyRef]
    _size += 1
    if (_size >= threshold) resize(len)
    None
  }
  
  def +=(kv: (K, V)) = { put(kv._1, kv._2); this }

  override def hashCode: Int = {
    val len = table.length
    var res = 0
    var i = 0
    while (i < len) {
      val key = table(i)
      if (key ne null) {
        val k = unmaskNull(key)
        res += System.identityHashCode(k) ^ System.identityHashCode(table(i + 1))
      }
      i += 2
    }
    res
  }

  override def clone: EqHashMap[K,V] = new EqHashMap[K,V](this)

  protected abstract class EqHashMapIterator[A] extends Iterator[A] {
    val tab = table
    val len = tab.length
    var index = if (size != 0) 0 else len
    var lastReturnedIndex = -1
    var indexValid = false

    def hasNext: Boolean = {
      var i = index
      while (i < len) {
        val key = tab(i)
        if (key ne null) {
          index = i
          indexValid = true
          return true
        }
        i += 2
      }
      index = len
      false
    }

    protected def nextIndex: Int = {
      if (!indexValid && !hasNext) throw new NoSuchElementException()

      indexValid = false;
      lastReturnedIndex = index;
      index += 2;
      return lastReturnedIndex;
    }
  }

  protected class KeyIterator extends EqHashMapIterator[K] {
    def next: K = unmaskNull(tab(nextIndex))
  }
  
  override def keysIterator: Iterator[K] = new KeyIterator

  protected class ValueIterator extends EqHashMapIterator[V] {
    def next: V = tab(nextIndex + 1)
  }

  override def valuesIterator: Iterator[V] = new ValueIterator
  
  protected class EntryIterator extends EqHashMapIterator[(K,V)] {
    def next: (K, V) = {
      nextIndex
      (unmaskNull(tab(lastReturnedIndex)), tab(lastReturnedIndex + 1))
    }
  }
  
  def iterator: Iterator[(K, V)] = new EntryIterator
}
object EqHashMap {
  private final val defCapacity = 32;
  private final val minCapacity = 8;
  private final val maxCapacity = 1 << 29;

  private[mutable] def capacity(sizeHint: Int): Int = {
    val min = 3 * sizeHint / 2
    if (min < minCapacity) minCapacity
    else if (min > maxCapacity || min < 0) maxCapacity
    else powerOf2(min)
  }

  private val _emptyKey = new AnyRef

  @inline final private[mutable] def hash(o: AnyRef, length: Int): Int = {
    val h = System.identityHashCode(o)
    // Multiply by -127 and left-shift to use least bit as part of hash
    ((h << 1) - (h << 8)) & (length - 1)
  }

  @inline final private[mutable] def nextKeyIndex(i: Int, len: Int): Int =
    if (i + 2 < len) i + 2 else 0

  def apply[K <: AnyRef, V](elems : (K,V)*) = empty ++= elems
  def empty[K <: AnyRef, V]: EqHashMap[K,V] = empty(defCapacity)
  def empty[K <: AnyRef, V](sizeHint: Int): EqHashMap[K,V] = new EqHashMap[K,V](sizeHint)
}