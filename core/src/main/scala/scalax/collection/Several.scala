package scalax.collection

import scala.collection.immutable.Iterable

/** Iterable that is known to contain two or more elements.
  */
case class Several[+N](_1: N, _2: N, more: Iterable[N]) extends Iterable[N] {
  def iterator: Iterator[N] = Iterator.several(_1, _2, more)

  override def map[B](f: N => B): Several[B] = new Several(f(_1), f(_2), more.map(f))

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

  /** `Some` hyperedge if `ends` contains at least two elements, otherwise `None`.
    */
  def from[N](iterable: Iterable[N]): Option[Several[N]] =
    endsOption(iterable) match {
      case ends @ Some(_) => ends
      case None           => None
    }

  /** A hyperedge with these `ends`.
    * @throws IllegalArgumentException if `ends` has not two elements at least.
    */
  def fromUnsafe[N](iterable: Iterable[N]): Several[N] =
    endsOption(iterable) match {
      case Some(ends) => ends
      case None =>
        throw new IllegalArgumentException(
          s"'iterable' must have at least two elements but it has only ${iterable.size}."
        )
    }

  private def endsOption[N](iterable: Iterable[N]): Option[Several[N]] = {
    val it                = iterable.iterator
    def next(): Option[N] = if (it.hasNext) Some(it.next()) else None

    (next(), next()) match {
      case (Some(_1), Some(_2)) =>
        Some(
          new Several[N](_1, _2, iterable.drop(2)) {
            override def iterator: Iterator[N] = iterable.iterator
          }
        )
      case _ => None
    }
  }

  def unapply[N](ends: Several[N]): Option[Seq[N]] = Some(ends.toSeq)
}
