package scalax.collection

import scala.collection.immutable.Iterable

sealed trait OneOrMore[+A] extends Seq[A] {
  override def map[B](f: A => B): OneOrMore[B] =
    this match {
      case o: One[A]     => o.map(f)
      case s: Several[A] => s.map(f)
    }
}

object OneOrMore {

  def apply[A](_1: A, more: A*): OneOrMore[A] =
    if (more.isEmpty) One(_1)
    else Several(_1, more.head, more.tail: _*)

  /** `Some[One[A]]` if `iterable` contains exactly one element,
    * `Some[Several[A]]` if `iterable` contains at least two elements,
    * otherwise `None`.
    */
  def from[A](iterable: Iterable[A]): Option[OneOrMore[A]] = {
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

  /** `One[A]` if `iterable` contains exactly one element,
    * `Several[A]` if `iterable` contains at least two elements.
    * @throws IllegalArgumentException if `iterable` is empty.
    */
  def fromUnsafe[A](iterable: Iterable[A]): OneOrMore[A] =
    from(iterable).getOrElse(
      throw new IllegalArgumentException(
        "'iterable' must have at least one element but it is empty."
      )
    )

  def unapply[A](oneOrMore: OneOrMore[A]): Some[(A, Seq[A])] =
    Some(oneOrMore match {
      case s: Several[A] => (s._1, s._2 +: s.more.toSeq)
      case One(_1)       => (_1, Nil)
    })
}

final case class One[+A](_1: A) extends OneOrMore[A] {
  def iterator: Iterator[A] = Iterator.single(_1)

  def apply(i: Int): A =
    if (i == 0) _1
    else throw new IndexOutOfBoundsException

  def length: Int = 1

  override def map[B](f: A => B): One[B] = One(f(_1))

  override def reverse: One[A] = this

  override def knownSize: Int = 1

  override def empty: One[A]    = ???
  override def isEmpty: Boolean = false

  override def head: A               = _1
  override def headOption: Option[A] = Some(_1)

  override def tail: Seq[A] = Nil
}

/** Seq that is known to contain at least two elements.
  */
final case class Several[+A](_1: A, _2: A, more: Iterable[A]) extends OneOrMore[A] {
  def iterator: Iterator[A] = Iterator.several(_1, _2, more)

  def apply(i: Int): A =
    if (i == 0) _1
    else if (i == 1) _2
    else if (i < 0) throw new IndexOutOfBoundsException
    else more.iterator.drop(i - 2).next()

  def length: Int = {
    val known = knownSize
    if (known == -1) more.size + 2
    else known
  }

  override def map[B](f: A => B): Several[B] = new Several(f(_1), f(_2), more.map(f))

  override def knownSize: Int = {
    val moreSize = more.knownSize
    if (moreSize == -1) -1
    else moreSize + 2
  }

  override def reverse: Several[A] = Several.fromUnsafe(toList.reverse)

  override def empty: Seq[A]    = ???
  override def isEmpty: Boolean = false

  override def head: A               = _1
  override def headOption: Option[A] = Some(_1)
}

object Several {
  def apply[A](_1: A, _2: A, more: A*): Several[A] = Several(_1, _2, more)

  /** `Some` `Several` if `iterable` contains at least two elements, otherwise `None`.
    */
  def from[A](iterable: Iterable[A]): Option[Several[A]] = {
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
  def fromUnsafe[A](iterable: Iterable[A]): Several[A] =
    from(iterable).getOrElse(
      throw new IllegalArgumentException(
        s"'iterable' must have at least two elements but it has ${iterable.size}."
      )
    )
}
