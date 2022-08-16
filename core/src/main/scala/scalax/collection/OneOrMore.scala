package scalax.collection

import scala.collection.immutable.Iterable

sealed trait OneOrMore[+N] extends Seq[N] {
  override def map[B](f: N => B): OneOrMore[B] = ??? // needs dummy implementation to override existing implementation
}

object OneOrMore {

  def apply[N](_1: N, more: N*): OneOrMore[N] =
    if (more.isEmpty) One(_1)
    else Several(_1, more.head, more.tail: _*)

  /** `Some` `One` if `iterable` contains exactly one element,
    * `Some` `Several` if `iterable` contains at least two elements,
    * otherwise `None`.
    */
  def from[N](iterable: Iterable[N]): Option[OneOrMore[N]] = {
    val it = iterable.iterator
    if (it.hasNext) Some {
      val _1 = it.next()
      if (it.hasNext) {
        val _2 = it.next()
        Several(_1, _2, iterable.drop(2).toSeq)
      } else One(_1)
    }
    else None
  }

  /** `One` if `iterable` contains exactly one element,
    * `Several` if `iterable` contains at least two elements.
    * @throws IllegalArgumentException if `iterable` is empty.
    */
  def fromUnsafe[N](iterable: Iterable[N]): OneOrMore[N] =
    from(iterable).getOrElse(
      throw new IllegalArgumentException(
        "'iterable' must have at least one element but it is empty."
      )
    )

  def unapply[N](oneOrMore: OneOrMore[N]): Some[(N, Seq[N])] =
    Some(oneOrMore match {
      case s: Several[N] => (s._1, s._2 +: s.more.toSeq)
      case One(_1)       => (_1, Nil)
    })
}

final case class One[+N](_1: N) extends OneOrMore[N] {
  def iterator: Iterator[N] = Iterator.single(_1)

  def apply(i: Int): N =
    if (i == 0) _1
    else throw new IndexOutOfBoundsException

  def length: Int = 1

  override def map[B](f: N => B): One[B] = One(f(_1))

  override def reverse: One[N] = this

  override def knownSize: Int = 1

  override def empty: One[N]    = ???
  override def isEmpty: Boolean = false

  override def head: N               = _1
  override def headOption: Option[N] = Some(_1)

  override def tail: Seq[N] = Nil
}

/** Seq that is known to contain at least two elements.
  */
final case class Several[+N](_1: N, _2: N, more: Iterable[N]) extends OneOrMore[N] {
  def iterator: Iterator[N] = Iterator.several(_1, _2, more)

  def apply(i: Int): N =
    if (i == 0) _1
    else if (i == 1) _2
    else if (i < 0) throw new IndexOutOfBoundsException
    else more.iterator.drop(i - 2).next()

  def length: Int = {
    val known = knownSize
    if (known == -1) more.size + 2
    else known
  }

  override def map[B](f: N => B): Several[B] = new Several(f(_1), f(_2), more.map(f))

  override def knownSize: Int = {
    val moreSize = more.knownSize
    if (moreSize == -1) -1
    else moreSize + 2
  }

  override def reverse: Several[N] = Several.fromUnsafe(toList.reverse)

  override def empty: Seq[N]    = ???
  override def isEmpty: Boolean = false

  override def head: N               = _1
  override def headOption: Option[N] = Some(_1)
}

object Several {
  def apply[N](_1: N, _2: N, more: N*): Several[N] = Several(_1, _2, more)

  /** `Some` `Several` if `iterable` contains at least two elements, otherwise `None`.
    */
  def from[N](iterable: Iterable[N]): Option[Several[N]] = {
    val it = iterable.iterator
    if (it.hasNext) {
      val _1 = it.next()
      if (it.hasNext) {
        val _2 = it.next()
        Some(Several(_1, _2, iterable.drop(2).toSeq))
      } else None
    } else None
  }

  /** A `Several` with the elements of `iterable`.
    * @throws IllegalArgumentException if `iterable` has not two elements at least.
    */
  def fromUnsafe[N](iterable: Iterable[N]): Several[N] =
    from(iterable).getOrElse(
      throw new IllegalArgumentException(
        s"'iterable' must have at least two elements but it has ${iterable.size}."
      )
    )
}
