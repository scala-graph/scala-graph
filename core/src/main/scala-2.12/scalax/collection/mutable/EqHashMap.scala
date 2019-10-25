package scalax.collection.mutable

import scala.annotation.switch
import scala.collection.mutable.{Map, MapLike}
import scala.language.implicitConversions

class EqHashMap[K <: AnyRef, V](_sizeHint: Int = EqHash.defCapacity)
    extends Map[K, V]
    with MapLike[K, V, EqHashMap[K, V]]
    with EqHash[(K, V), EqHashMap[K, V]] {

  import EqHash.{defCapacity, evenHash}

  final protected def sizeHint: Int = _sizeHint
  final protected def step          = 2

  def this(other: EqHashMap[K, V]) {
    this()
    from(other)
  }

  @inline implicit final private def toK(o: AnyRef): K = o.asInstanceOf[K]
  @inline implicit final private def toV(o: AnyRef): V = o.asInstanceOf[V]

  override def empty: EqHashMap[K, V] = EqHashMap.empty[K, V](defCapacity)

  def getOrNull(key: K): AnyRef = (index(key): @switch) match {
    case i if i < 0 => null
    case i          => table(i + 1)
  }

  @inline final override def apply(key: K): V = {
    val value = getOrNull(key)
    if (value eq null) default(key)
    else value
  }

  @inline final def get(key: K): Option[V] = Option(getOrNull(key))

  @inline final override def contains(key: K): Boolean = getOrNull(key) ne null

  override def remove(key: K): Option[V] = (index(key): @switch) match {
    case i if i < 0 => None
    case i =>
      _size -= 1
      val oldValue = table(i + 1)
      table(i + 1) = null
      table(i) = null
      closeDeletion(i)
      Some(oldValue)
  }

  def -=(key: K) = { remove(key); this }

  protected def move(oldTable: Array[AnyRef], oldLength: Int, newTable: Array[AnyRef], newLength: Int): Unit = {
    var j = 0
    while (j < oldLength) {
      val key = oldTable(j);
      if (key ne null) {
        val value = oldTable(j + 1)
        oldTable(j) = null
        oldTable(j + 1) = null
        var i = evenHash(key, newLength)
        while (newTable(i) ne null) i = nextKeyIndex(i, newLength)
        newTable(i) = key
        newTable(i + 1) = value
      }
      j += step
    }
  }

  override def put(key: K, value: V): Option[V] = {
    val maskedKey = maskNull(key)
    val tab       = table
    val len       = tab.length
    val keyHash   = evenHash(maskedKey, len)
    (index(maskedKey, keyHash, len): @switch) match {
      case i if i < 0 =>
        val neg = ~i
        tab(neg) = maskedKey
        tab(neg + 1) = value.asInstanceOf[AnyRef]
        _size += 1
        if (_size >= threshold) resize
        None
      case i =>
        val oldValue = tab(i + 1)
        tab(i) = maskedKey
        tab(i + 1) = value.asInstanceOf[AnyRef]
        Some(oldValue)
    }
  }

  def +=(kv: (K, V)) = { put(kv._1, kv._2); this }

  override protected def elemHashCode: (Array[AnyRef], Int) => Int = (tab, i) => {
    val k = unmaskNull(tab(i))
    System.identityHashCode(k) ^ System.identityHashCode(tab(i + 1))
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[EqHashMap[K, V]]

  override def containsElem(elem: (K, V)): Boolean = (index(elem._1): @switch) match {
    case i if i < 0 => false
    case i          => table(i + 1) == elem._2
  }

  override def clone: EqHashMap[K, V] = new EqHashMap[K, V](this)

  protected class KeyIterator extends EqHashIterator[K] {
    def next: K = unmaskNull(tab(nextIndex))
  }

  override def keysIterator: Iterator[K] = new KeyIterator

  protected class ValueIterator extends EqHashIterator[V] {
    def next: V = tab(nextIndex + 1)
  }

  override def valuesIterator: Iterator[V] = new ValueIterator

  protected class EntryIterator extends EqHashIterator[(K, V)] {
    def next: (K, V) = {
      val i = nextIndex
      (unmaskNull(tab(i)), tab(i + 1))
    }
  }

  def iterator: Iterator[(K, V)] = new EntryIterator
}

object EqHashMap {

  import EqHash._

  def apply[K <: AnyRef, V](elems: (K, V)*)                 = empty ++= elems
  def empty[K <: AnyRef, V]: EqHashMap[K, V]                = empty(defCapacity)
  def empty[K <: AnyRef, V](sizeHint: Int): EqHashMap[K, V] = new EqHashMap[K, V](sizeHint)
}
