package scalax.collection

import scala.collection.immutable.Iterable

sealed trait OneOrMore[+N] extends Iterable[N] {
  override def map[B](f: N => B): OneOrMore[B] = ??? // needs dummy implementation to override existing implementation
  def reverse: OneOrMore[N]
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

  def unapply[N](oneOrMore: OneOrMore[N]): Option[Seq[N]] =
    Some(oneOrMore match {
      case s: Several[N] => s.iterator.toSeq
      case One(_1)       => _1 :: Nil
    })
}

final case class One[+N](_1: N) extends OneOrMore[N] {
  def iterator: Iterator[N] = Iterator.single(_1)

  override def map[B](f: N => B): One[B] = One(f(_1))

  def reverse: One[N] = this

  override def concat[B >: N](suffix: IterableOnce[B]): OneOrMore[B] =
    if (suffix.iterator.isEmpty) this
    else Several(_1, suffix.iterator.next(), suffix.iterator.toSeq.drop(1))

  override def knownSize: Int = 1

  final override def empty: Iterable[N] = ???
  final override def isEmpty: Boolean   = false

  override def head: N               = _1
  override def headOption: Option[N] = Some(_1)

  override def tail: Iterable[N] = Nil
}

/** Iterable that is known to contain at least two elements.
  */
final case class Several[+N](_1: N, _2: N, more: Iterable[N]) extends OneOrMore[N] {
  def iterator: Iterator[N] = Iterator.several(_1, _2, more)

  override def map[B](f: N => B): Several[B] = new Several(f(_1), f(_2), more.map(f))

  def reverse: Several[N] = Several.fromUnsafe(toList.reverse)

  override def concat[B >: N](suffix: IterableOnce[B]): Several[B] = copy(more = this.more ++ suffix)

  override def knownSize: Int = {
    val moreSize = more.knownSize
    if (moreSize == -1) -1
    else moreSize + 2
  }

  final override def empty: Iterable[N] = ???
  final override def isEmpty: Boolean   = false

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

  def unapply[N](several: Several[N]): Option[Seq[N]] = Some(several.toSeq)
}
