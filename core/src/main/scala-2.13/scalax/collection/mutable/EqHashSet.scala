package scalax.collection.mutable

import scala.annotation.switch
import scala.collection.mutable.Set

class EqHashSet[A <: AnyRef](_sizeHint: Int = EqHash.defCapacity)
    extends Set[A]
    with EqHash[A, EqHashSet[A]] {

  import EqHash.{anyHash, defCapacity}

  final protected def sizeHint: Int = _sizeHint
  final protected def step          = 1

  def this(other: EqHashSet[A]) {
    this()
    from(other)
  }

  override def empty: EqHashSet[A] = EqHashSet.empty[A](defCapacity)

  @inline final override def contains(elem: A): Boolean = index(elem) >= 0

  override def addOne(elem: A): this.type = { add(elem); this }

  override def add(elem: A): Boolean = {
    val masked  = maskNull(elem)
    val tab     = table
    val len     = tab.length
    val keyHash = anyHash(masked, len)
    (index(masked, keyHash, len): @switch) match {
      case i if i < 0 =>
        val neg = ~i
        tab(neg) = masked
        _size += 1
        if (_size >= threshold) resize
        true
      case i => false
    }
  }

  protected def move(oldTable: Array[AnyRef], oldLength: Int, newTable: Array[AnyRef], newLength: Int): Unit = {
    var j = 0
    while (j < oldLength) {
      val key = oldTable(j);
      if (key ne null) {
        oldTable(j) = null
        var i = anyHash(key, newLength)
        while (newTable(i) ne null) i = nextKeyIndex(i, newLength)
        newTable(i) = key
      }
      j += step
    }
  }

  override def subtractOne(elem: A): this.type = { remove(elem); this }

  override def remove(elem: A): Boolean = (index(elem): @switch) match {
    case i if i < 0 => false
    case i =>
      _size -= 1
      table(i) = null
      closeDeletion(i)
      true
  }

  override protected def elemHashCode: (Array[AnyRef], Int) => Int =
    (tab, i) => System.identityHashCode(unmaskNull(tab(i)))

  override def canEqual(other: Any): Boolean = other.isInstanceOf[EqHashSet[A]]

  override def containsElem(elem: A): Boolean = index(elem) >= 0

  override def clone: EqHashSet[A] = new EqHashSet[A](this)

  def iterator: Iterator[A] = new EqHashIterator[A] {
    def next: A = {
      val i = nextIndex
      unmaskNull(tab(i)).asInstanceOf[A]
    }
  }
}

object EqHashSet {

  import EqHash._

  def apply[A <: AnyRef](elems: A*)                   = empty ++= elems
  def empty[A <: AnyRef]: EqHashSet[A]                = empty(sizeHint = defCapacity)
  def empty[A <: AnyRef](sizeHint: Int): EqHashSet[A] = new EqHashSet[A](sizeHint)
}
