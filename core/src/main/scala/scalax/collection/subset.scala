package scalax.collection

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.util.chaining._

protected trait SubsetOps[A, +S <: Set[A] with Singleton] extends Set[A] {
  def superset: S
  def set: Set[A]

  def contains(elem: A): Boolean = set(elem)
  def iterator: Iterator[A]      = set.iterator
  override def toString: String  = s"${getClass.getSimpleName}(${set mkString ", "}) out of ${superset.size}"
}

/** Includes a `superset` and a `set` guaranteeing that `set` is a subset of `superset`.
  *
  * @tparam A type of the elements of both `superset` and `set`
  * @tparam S singleton type denoting `superset`
  */
case class Subset[A, +S <: Set[A] with Singleton] private (superset: S, set: Set[A]) extends SubsetOps[A, S] {

  def incl(elem: A): Subset[A, S] =
    if (superset(elem) && !set(elem)) Subset(superset, set incl elem)
    else this

  def excl(elem: A): Subset[A, S] = Subset(superset, set excl elem)
}

object Subset {
  def apply[A](superset: Set[A])(elems: A*): Subset[A, superset.type] =
    new Subset(superset, elems.iterator.filter(superset.apply).toSet)

  def byFilter[A](superset: Set[A])(predicate: A => Boolean): Subset[A, superset.type] =
    new Subset(superset, superset.filter(predicate))
}

/** Includes a `superset`, a `seq` and a `set` guaranteeing that `set` is a subset of `superset` and
  * `seq` has the same elements as `set` ordered on creation time.
  *
  * @tparam A type of the elements of both `superset` and `set`
  * @tparam S singleton type denoting `superset`
  */
case class OrderedSubset[A, +S <: Set[A] with Singleton] private (superset: S, ordered: ArraySeq[A], set: Set[A])
    extends SubsetOps[A, S] {

  def incl(elem: A): OrderedSubset[A, S] =
    if (superset(elem) && !set(elem))
      OrderedSubset(superset, ordered appended elem, set incl elem)
    else this

  def excl(elem: A): OrderedSubset[A, S] =
    ordered.indexOf(elem) pipe { idx =>
      if (idx == -1) this
      else OrderedSubset(superset, ordered.patch(idx, elem :: Nil, 1), set excl elem)
    }
}

object OrderedSubset {
  def apply[A](superset: Set[A])(elems: A*)(implicit tag: ClassTag[A]): OrderedSubset[A, superset.type] =
    elems.foldLeft((ArraySeq.empty[A], Set.empty[A])) {
      case (cum @ (_, set), elem) if !superset(elem) || set(elem) => cum
      case ((seq, set), elem)                                     => (seq appended elem, set incl elem)
    } match {
      case (seq, set) => new OrderedSubset(superset, seq, set)
    }
}

/** Includes a `superset` and a single element `set` guaranteeing that `set` is a subset of `superset`.
  *
  * @tparam A type of the elements of both `superset` and `set`
  * @tparam S singleton type denoting `superset`
  */
case class OneOf[A, +S <: Set[A] with Singleton] private (superset: S, one: A) {
  override def toString: String = s"One($one) out of ${superset.size}"
}

object OneOf {
  def apply[A](superset: Set[A])(elem: A): Option[OneOf[A, superset.type]] =
    if (superset(elem)) Some(new OneOf(superset, elem))
    else None
}

/** Wraps an `immutable.Set` by also capturing the underlying set as a singleton type.
  * Useful if you need to enforce `Superset` and `Subset` instances to use the same superset singleton.
  * Gets probably obsolete in Scala 3.
  */
case class Superset[A, +S <: Set[A] with Singleton](set: Set[A] with S)
