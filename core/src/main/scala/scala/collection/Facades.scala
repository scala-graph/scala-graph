package scala.collection

/** Wraps `t` to a [[scala.collection.Seq Seq]]. It helps to avoid the creation of a copy
  *  of the elements of `t` when passing `t` to repeated parameters of type `A`.
  *  `apply` is O(N).
  *
  * @param t the underlying `Traversable`.
  */
final class SeqFacade[A](t: Traversable[A]) extends Seq[A] {
  def iterator: Iterator[A] = t.toIterator
  def apply(idx: Int): A = {
    val it = iterator
    var i  = 0
    while (i < idx) { it.next; i += 1 }
    it.next
  }
  def length: Int = t.size
}

/** Wraps the [[scala.collection.Traversable Traversable]] `t` to a
  *  [[scala.collection.immutable.Set Set]].
  *  It aims at efficiently creating a set in case the caller ensures that
  *  all elements in `t` are unique.
  *  `+` and `-` are O(N) returning [[scala.collection.immutable.Set]].
  *
  * @param t the underlying `Traversable` with unique elements.
  */
class SetFacade[A](t: Traversable[A]) extends immutable.Set[A] {
  def contains(elem: A)           = t exists (_ == elem)
  final def iterator: Iterator[A] = t.toIterator
  final def -(elem: A)            = t.toSet - elem
  final def +(elem: A)            = t.toSet + elem

  final override def size: Int = t.size
  // avoids IterableLike's foreach that is based on iterator
  final override def foreach[U](f: (A) => U): Unit = t foreach f
}

/** Wraps the [[scala.collection.Traversable Traversable]] `t` to a
  *  [[scala.collection.immutable.Set Set]] utilizing reference equality.
  *  It aims at efficiently creating a set in case the caller ensures that
  *  all elements in `t` are unique.
  *  `+` and `-` are O(N) returning [[scala.collection.immutable.Set]].
  *
  * @param t the underlying `Traversable` with unique elements.
  */
final class EqSetFacade[A <: AnyRef](t: Traversable[A]) extends SetFacade[A](t) {
  final override def contains(elem: A) = t exists (_ eq elem)
}

/** Template for sets having a `withFilter` that keeps `Set` semantics.
  *  This class becomes obsolete as soon as `scala.collection.Set.withFilter`
  *  returns a `Set` instead of just `FilterMonadic`.
  */
trait FilterableSet[A] {
  this: FilteredSet[A] =>

  def withFilter(q: (A) => Boolean): immutable.Set[A] with FilterableSet[A] =
    new FilteredSet(set, elem => p(elem) && q(elem))
}

/** A `Set` implementation extended by `FilterableSet`.
  */
final class FilteredSet[A](val set: Set[A], val p: (A) => Boolean) extends immutable.Set[A] with FilterableSet[A] {
  def contains(elem: A)     = p(elem) && (set contains elem)
  def iterator: Iterator[A] = set.iterator withFilter p
  def -(elem: A)            = if (contains(elem)) iterator.toSet - elem else this
  def +(elem: A)            = if (contains(elem)) this else iterator.toSet + elem

  override def withFilter(q: (A) => Boolean): immutable.Set[A] with FilterableSet[A] =
    super[FilterableSet].withFilter(q)
}
