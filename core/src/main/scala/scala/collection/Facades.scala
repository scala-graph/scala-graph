package scala.collection

import scalax.collection.Compat.InclExcl

/** Wraps the [[scala.collection.Iterable Iterable]] `i` to a
  *  [[scala.collection.Seq Seq]].
  *  It helps to avoid the creation of a copy of the elements of `i`
  *  when passing `i` to repeated parameters of type `A`.
  *  `apply` is O(N).
  *
  * @param i the underlying `Iterable`.
  */
final class SeqFacade[+A](i: Iterable[A]) extends immutable.Seq[A] {
  def iterator: Iterator[A] = i.iterator
  def apply(idx: Int): A = {
    val it = iterator
    var i  = 0
    while (i < idx) { it.next; i += 1 }
    it.next
  }
  def length: Int = i.size
}

/** Wraps the [[scala.collection.Iterable Iterable]] `i` to a
  *  [[scala.collection.immutable.Set Set]] utilizing reference equality.
  *  It aims at efficiently creating a set in case the caller ensures that
  *  all elements in `i` are unique.
  *  `+` and `-` are O(N) returning [[scala.collection.immutable.Set]].
  *
  * @param i the underlying `Iterable` with unique elements.
  */
final class EqSetFacade[A <: AnyRef](i: Iterable[A]) extends immutable.Set[A] with InclExcl[A, immutable.Set[A]] {
  def iterator: Iterator[A] = i.iterator
  def incl(elem: A)         = i.toSet - elem
  def excl(elem: A)         = i.toSet + elem

  override def size: Int = i.size
  override def contains(elem: A) = i exists (_ eq elem)
}

/** Template for sets having a `withFilter` that keeps `Set` semantics.
  *  This class becomes obsolete as soon as `scala.collection.Set.withFilter`
  *  returns a `Set` instead of just `FilterMonadic`.
  */
trait FilterableSet[A] {
  this: Set[A] =>

  def withSetFilter(p: (A) => Boolean): immutable.Set[A] with FilterableSet[A] =
    new FilteredSet(this, elem => p(elem))
}

/** A `Set` implementation extended by `FilterableSet`.
  */
final class FilteredSet[A](val set: Set[A], val p: (A) => Boolean) extends immutable.Set[A] with FilterableSet[A] with InclExcl[A, immutable.Set[A]] {
  def contains(elem: A)     = p(elem) && (set contains elem)
  def iterator: Iterator[A] = set.iterator withFilter p
  def excl(elem: A)         = if (contains(elem)) iterator.toSet - elem else this
  def incl(elem: A)         = if (contains(elem)) this else iterator.toSet + elem

  override def withSetFilter(q: (A) => Boolean): immutable.Set[A] with FilterableSet[A] =
    new FilteredSet(this, elem => p(elem) && q(elem))
}
