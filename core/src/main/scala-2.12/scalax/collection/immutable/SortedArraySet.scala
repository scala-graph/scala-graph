package scalax.collection.immutable

import scala.collection.{AbstractIterator, SortedSetLike}
import scala.collection.immutable.SortedSet
import scala.collection.generic.{CanBuildFrom, SortedSetFactory}
import compat.Platform.arraycopy

@SerialVersionUID(1L)
class SortedArraySet[A](array: Array[A] = new Array[AnyRef](0).asInstanceOf[Array[A]])(
    implicit val ordering: Ordering[A])
    extends SortedSet[A]
    with SortedSetLike[A, SortedArraySet[A]]
    with Serializable { self =>
  java.util.Arrays.sort(array.asInstanceOf[Array[AnyRef]], ordering.asInstanceOf[Ordering[Object]])

  override def empty = SortedArraySet.empty

  override def +(elem: A): SortedArraySet[A] =
    if (contains(elem)) this
    else {
      val newSize = size + 1
      val newArr  = new Array[AnyRef](newSize).asInstanceOf[Array[A]]
      arraycopy(array, 0, newArr, 0, size)
      newArr(size) = elem
      new SortedArraySet(newArr)
    }

  override def -(elem: A): SortedArraySet[A] = {
    val idx = array.indexOf(elem)
    if (idx == -1) this
    else {
      val newSize = size - 1
      val newArr  = new Array[AnyRef](newSize).asInstanceOf[Array[A]]
      if (idx > 0) arraycopy(array, 0, newArr, 0, idx)
      if (idx < newSize) arraycopy(array, idx + 1, newArr, idx, newSize - idx)
      new SortedArraySet(newArr)
    }
  }

  def keysIteratorFrom(start: A): Iterator[A] =
    search(start, ordering.lt) map iterator getOrElse Iterator.empty

  def contains(elem: A): Boolean = array.indexOf(elem) >= 0

  final protected def iterator(from: Int): Iterator[A] =
    new AbstractIterator[A] {
      private[this] var i = from
      def hasNext         = i < self.size
      def next            = { val elm = array(i); i += 1; elm }
    }

  def iterator: Iterator[A] = iterator(0)

  final protected def search(elem: A, cond: (A, A) => Boolean): Option[Int] = {
    var i     = 0
    var found = -1
    while (found == -1 && i < size) if (cond(array(i), elem)) i += 1
    else found = i
    if (found == -1) None else Some(found)
  }

  def rangeImpl(from: Option[A], until: Option[A]): SortedArraySet[A] = {
    if (size == 0 || (from == None && until == None)) return this
    val idxFrom = from flatMap (search(_, ordering.lt)) getOrElse 0
    val idxTill = (until flatMap (e =>
      search(e, ordering.lt) orElse (
        if (ordering.gt(e, array(size - 1))) Some(size)
        else Some(-1)
      )) getOrElse size) - 1
    if (idxFrom > idxTill) empty
    else {
      val newSize               = idxTill - idxFrom + 1
      val newArr: Array[AnyRef] = new Array(newSize)
      arraycopy(array, idxFrom, newArr, 0, newSize)
      new SortedArraySet(newArr.asInstanceOf[Array[A]])
    }
  }

  def find(elem: A): Option[A] = {
    val i = array.indexOf(elem)
    if (i >= 0) Some(array(i)) else None
  }

  override def size: Int = array.length
}
object SortedArraySet extends SortedSetFactory[SortedArraySet] {
  implicit def canBuildFrom[A](implicit ordering: Ordering[A]): CanBuildFrom[Coll, A, SortedArraySet[A]] =
    newCanBuildFrom[A]
  override def empty[A](implicit ordering: Ordering[A]): SortedArraySet[A] =
    new SortedArraySet[A](new Array[AnyRef](0).asInstanceOf[Array[A]])
}
