package scalax.collection

import scala.collection.{IterableFactory, IterableOnce}
import scala.collection.immutable.Iterable
import scala.collection.mutable.Buffer
import scala.util.chaining._

protected trait UnchangedSizeOps[+A, CC[+X] <: NonEmpty[X]] {
  def iterator: Iterator[A]

  /** The element at `index` starting with 0 looked up in a non-safe way.
    * @throws IndexOutOfBoundsException if `index` is negative or greater or equal to this collection's size.
    */
  def apply(index: Int): A

  /** Whether an element at `index` exists. */
  def isDefinedAt(index: Int): Boolean

  /** Whether `elem` is included in this collection. */
  def contains[B >: A](elem: B): Boolean = exists(_ == elem)

  /** Whether an element satisfying the predicate `p` exists. */
  def exists(p: A => Boolean): Boolean = find(p).isDefined

  /** Finds an element satisfying the predicate `p` if any. */
  def find(p: A => Boolean): Option[A]

  /** Whether all elements satisfy the predicate `p`. */
  def forall(p: A => Boolean): Boolean

  /** Calls `f` on all elements of this collection for side effects. */
  def foreach[U](f: A => U): Unit

  /** Gets the element at `index` if any. */
  def get(index: Int): Option[A] = if (isDefinedAt(index)) Some(apply(index)) else None

  /** The first element. */
  def head: A

  /** The number of elements in this collection. */
  def size: Int

  /** Applies `f` to a start value and all elements of this collection, going left to right. */
  def foldLeft[B](zero: B)(f: (B, A) => B): B = iterator.foldLeft(zero)(f)

  /** Applies `f` to all elements of this collection, going left to right. */
  def reduceLeft[B >: A](f: (B, A) => B): B = iterator.reduceLeft(f)

  /** Builds a new collection by applying a function to all elements of this collection. */
  def map[B](f: A => B): CC[B]

  final def reverse: CC[A]  = newUnsafeBuilder.result(iterator.toList.reverse)
  final def toList: List[A] = iterator.toList

  final def zip[B](that: IterableOnce[B]): CC[(A, B)] = newUnsafeBuilder.result(iterator zip that)
  final def zip[B](that: NonEmpty[B]): CC[(A, B)]     = newUnsafeBuilder.result(iterator zip that.iterator)
  final def zipWithIndex: CC[(A, Int)]                = newUnsafeBuilder.result(iterator.zipWithIndex)

  protected def apply[B >: A](index: Int, more: Iterable[B], start: Int): B =
    if (index < 0) throw new IndexOutOfBoundsException
    else
      try more.iterator.drop(index - start).next()
      catch {
        case _: Exception => throw new IndexOutOfBoundsException
      }

  protected def newUnsafeBuilder[B]: NonEmptyBuilder.Unsafe[B, CC]
}

protected trait LengtheningOps[+A, CC[+X] <: NonEmpty[X]] {
  this: UnchangedSizeOps[A, CC] =>

  /** Concatenates the elements of this collection and those of `suffix` into a new collection. */
  def concat[B >: A](suffix: IterableOnce[B]): CC[B] = appendedAll(suffix)

  /** Alias for `concat` */
  final def ++[B >: A](that: IterableOnce[B]): CC[B] = concat(that)

  /** Alias for `concat` */
  final def ++[B >: A](that: NonEmpty[B]): CC[B] = concat(that.iterator)

  /** Creates a new collection with `elem` as its first element followed by the elements of this collection. */
  def prepended[B >: A](elem: B): CC[B]

  /** Creates a new collection with the elements of this collection and `elem` appended. */
  def appended[B >: A](elem: B): CC[B]

  /** Alias for `prepended` */
  final def +:[B >: A](elem: B): CC[B] = prepended(elem)

  /** Alias for `appended` */
  final def :+[B >: A](elem: B): CC[B] = appended(elem)

  final def prependedAll[B >: A](prefix: IterableOnce[B]): CC[B] = newUnsafeBuilder.result(prefix.iterator ++ iterator)
  final def prependedAll[B >: A](prefix: NonEmpty[B]): CC[B]     = prependedAll(prefix.iterator)

  /** Alias for `prependedAll` */
  final def ++:[B >: A](prefix: IterableOnce[B]): CC[B] = prependedAll(prefix)

  /** Alias for `prependedAll` */
  final def ++:[B >: A](prefix: NonEmpty[B]): CC[B] = prependedAll(prefix.iterator)

  final def appendedAll[B >: A](suffix: IterableOnce[B]): CC[B] = newUnsafeBuilder.result(iterator ++ suffix.iterator)
  final def appendedAll[B >: A](suffix: NonEmpty[B]): CC[B]     = appendedAll(suffix.iterator)

  /** Alias for `appendedAll` */
  final def :++[B >: A](suffix: IterableOnce[B]): CC[B] = appendedAll(suffix)

  /** Alias for `appendedAll` */
  final def :++[B >: A](suffix: NonEmpty[B]): CC[B] = appendedAll(suffix.iterator)
}

protected trait ShorteningOps[+A, CC[+X] <: NonEmpty[X], SCC[+_] <: Iterable[_]] {
  this: UnchangedSizeOps[A, CC] =>
  protected def newEscapingBuilder[B]: NonEmptyBuilder.Escaping[B, SCC]

  /** Creates a `List` with all elements of this collection that satisfy the predicate `p` */
  final def filter(p: A => Boolean): SCC[A] = newEscapingBuilder.result(iterator filter p)

  /** Creates a `List` with all elements of this collection that don't satisfy the predicate `p` */
  final def filterNot(p: A => Boolean): SCC[A] = filter(!p(_))

  /** Creates a `List` with all elements of this collection that satisfy `pf` */
  final def collect[B](pf: PartialFunction[A, B]): SCC[B] = newEscapingBuilder.result(iterator collect pf)
}

/** @define ESCAPING creates a collection of the same type and returns it in a `Right`
  *                  unless the number of remaining elements is too small in which case it escapes to a `Left[List]`.
  */
protected trait ShorteningEitherOps[+A, CC[+X] <: NonEmpty[X], SCC[+_] <: Iterable[_]] {
  this: UnchangedSizeOps[A, CC] =>
  protected def newConditionalBuilder[B]: NonEmptyBuilder.Conditional[B, CC, SCC]

  /** Same as `filter` but $ESCAPING */
  def filterEither(p: A => Boolean): Either[SCC[A], CC[A]] = newConditionalBuilder.result(iterator filter p)

  /** Same as `filterNot` but $ESCAPING */
  def filterNotEither(p: A => Boolean): Either[SCC[A], CC[A]] = filterEither(!p(_))

  /** Same as `collect` but $ESCAPING */
  def collectEither[B](pf: PartialFunction[A, B]): Either[SCC[B], CC[B]] =
    newConditionalBuilder.result(iterator collect pf)
}

protected trait NonEmptyOps[+A, CC[+X] <: NonEmpty[X], SCC[+_] <: Iterable[_]]
    extends UnchangedSizeOps[A, CC]
    with LengtheningOps[A, CC]
    with ShorteningOps[A, CC, SCC]
    with ShorteningEitherOps[A, CC, SCC]

sealed trait NonEmpty[+A] {

  /** Iterates over all elements of this collection. */
  def iterator: Iterator[A]
}

protected trait NonEmptyFactory[CC[+X] <: NonEmpty[X]] {

  /** Tries to create a non-empty collection populated with the elements of `source`. */
  def from[A](source: IterableOnce[A]): Option[CC[A]]

  /** Creates a non-empty collection populated with the elements of `source` in a non-safe way.
    * @throws IllegalArgumentException if `source` does not contain enough elements.
    */
  def fromUnsafe[A](source: IterableOnce[A]): CC[A]

  /** Optional extractor to get a `Seq` extracted. */
  object Seq {
    def unapplySeq[A](nonEmpty: CC[A]): Some[Seq[A]] = Some(nonEmpty.iterator.to(scala.Seq))
  }
}

protected object NonEmptyBuilder {
  protected[collection] trait Builder[A, +R] {
    protected val buf              = Buffer.empty[A]
    def clear(): Unit              = buf.clear()
    def addOne(elem: A): this.type = { buf += elem; this }
    def +=(elem: A): this.type     = addOne(elem)

    def result(it: IterableOnce[A]): R
  }

  protected[collection] class Unsafe[A, CC[+X] <: NonEmpty[X]](factory: NonEmptyFactory[CC]) extends Builder[A, CC[A]] {
    private def from(it: IterableOnce[A]): CC[A] = factory.fromUnsafe(it).tap(_ => clear())

    def result(): CC[A]                    = from(buf)
    def result(it: IterableOnce[A]): CC[A] = from(it)
  }

  protected[collection] class Escaping[A, CC[+_]](factory: IterableFactory[CC]) extends Builder[A, CC[A]] {
    private def from(it: IterableOnce[A]): CC[A] = factory.from(it).tap(_ => clear())

    def result(): CC[A]                    = from(buf)
    def result(it: IterableOnce[A]): CC[A] = from(it)
  }

  protected[collection] class Conditional[A, CC[+X] <: NonEmpty[X], SCC[+_] <: Iterable[_]](
      minSize: Int,
      nonEmptyFactory: NonEmptyFactory[CC],
      escapingFactory: IterableFactory[SCC]
  ) extends Builder[A, Either[SCC[A], CC[A]]] {
    require(minSize > 0)

    def result(): Either[SCC[A], CC[A]] = {
      if (buf.size >= minSize) Right(nonEmptyFactory.fromUnsafe(buf))
      else Left(escapingFactory.from(buf))
    }.tap(_ => clear())

    def result(iterableOnce: IterableOnce[A]): Either[SCC[A], CC[A]] = {
      def lookAhead(): (Boolean, Iterator[A]) = iterableOnce match {
        case it: Iterator[A] =>
          var i = 0
          while (i < minSize && it.hasNext) {
            buf += it.next()
            i += 1
          }
          (i == minSize, buf.iterator ++ it)
        case iterable: Iterable[A] =>
          val it = iterable.iterator
          var i  = 0
          while (i < minSize && it.hasNext)
            i += 1
          (i == minSize, iterable.iterator)
      }

      (lookAhead() match {
        case (true, it)  => Right(nonEmptyFactory.fromUnsafe(it))
        case (false, it) => Left(escapingFactory.from(it))
      }).tap(_ => clear())
    }
  }
}

/** Collection of at least one element.
  */
final case class OneOrMore[+A](head: A, tail: Iterable[A]) extends NonEmpty[A] with NonEmptyOps[A, OneOrMore, List] {
  def iterator: Iterator[A] = Iterator.oneOrMore(head, tail)

  def find(p: A => Boolean): Option[A] =
    if (p(head)) Some(head)
    else tail find p

  def forall(p: A => Boolean): Boolean = p(head) && tail.forall(p)

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail.map(f)
  }

  def apply(index: Int): A =
    if (index == 0) head
    else apply(index, tail, 1)

  def isDefinedAt(index: Int): Boolean = index == 0 || index < tail.size + 1

  def size: Int = tail.size + 1

  def map[B](f: A => B): OneOrMore[B] = new OneOrMore(f(head), tail.map(f))

  def prepended[B >: A](elem: B): OneOrMore[B] = OneOrMore(elem, Iterable(head) concat tail)
  def appended[B >: A](elem: B): OneOrMore[B]  = OneOrMore(head, tail concat Iterator.single(elem))

  protected def newConditionalBuilder[B]: NonEmptyBuilder.Conditional[B, OneOrMore, List] =
    new NonEmptyBuilder.Conditional[B, OneOrMore, List](minSize = 1, OneOrMore, List)

  protected def newEscapingBuilder[B]: NonEmptyBuilder.Escaping[B, List] =
    new NonEmptyBuilder.Escaping[B, List](List)

  protected def newUnsafeBuilder[B]: NonEmptyBuilder.Unsafe[B, OneOrMore] =
    new NonEmptyBuilder.Unsafe[B, OneOrMore](OneOrMore)
}

object OneOrMore extends NonEmptyFactory[OneOrMore] {

  /** Factory to pass elements individually. */
  def apply[A](head: A, more: A*): OneOrMore[A] = OneOrMore(head, more)

  /** Factory to create a `OneOrMore` containing the single element `head`. */
  def one[A](head: A): OneOrMore[A] = OneOrMore(head, Nil)

  /** Factory to create a `OneOrMore` containing at leas the two elements `head` and `_2`. */
  def more[A](head: A, _2: A, more: A*): OneOrMore[A] = OneOrMore(head, Iterable(_2) concat more)

  /** `Some[OneOrMore]` populated with the elements of `source` if it contains at least one element, otherwise `None`.
    */
  def from[A](source: IterableOnce[A]): Option[OneOrMore[A]] = {
    val it = source.iterator
    if (it.hasNext) Some(OneOrMore(it.next(), it.toList))
    else None
  }

  /** Creates a `OneOrMore[A]` populated with the elements of `source` in a non-safe way.
    * @throws IllegalArgumentException if `source` is empty.
    */
  def fromUnsafe[A](source: IterableOnce[A]): OneOrMore[A] =
    from(Iterable.from(source)).getOrElse(
      throw new IllegalArgumentException("empty 'source' passed to OneOrMore.fromUnsafe")
    )
}

/** Collection of at least two elements.
  */
final case class Several[+A](head: A, _2: A, more: Iterable[A]) extends NonEmpty[A] with NonEmptyOps[A, Several, List] {
  def iterator: Iterator[A] = Iterator.several(head, _2, more)

  def find(p: A => Boolean): Option[A] =
    if (p(head)) Some(head)
    else if (p(_2)) Some(_2)
    else more find p

  def forall(p: A => Boolean): Boolean = p(head) && p(_2) && more.forall(p)

  def foreach[U](f: A => U): Unit = {
    f(head)
    f(_2)
    more.map(f)
  }

  def apply(index: Int): A =
    if (index == 0) head
    else if (index == 1) _2
    else apply(index, more, 2)

  def isDefinedAt(index: Int): Boolean = index == 0 || index == 1 || index < more.size + 2

  def size: Int = more.size + 2

  def map[B](f: A => B): Several[B] = new Several(f(head), f(_2), more.map(f))

  def prepended[B >: A](elem: B): Several[B] = Several(elem, head, Iterable(_2) concat more)
  def appended[B >: A](elem: B): Several[B]  = Several(head, _2, more concat Iterator.single(elem))

  protected def newConditionalBuilder[B]: NonEmptyBuilder.Conditional[B, Several, List] =
    new NonEmptyBuilder.Conditional[B, Several, List](minSize = 2, Several, List)

  protected def newEscapingBuilder[B]: NonEmptyBuilder.Escaping[B, List] =
    new NonEmptyBuilder.Escaping[B, List](List)

  protected def newUnsafeBuilder[B]: NonEmptyBuilder.Unsafe[B, Several] =
    new NonEmptyBuilder.Unsafe[B, Several](Several)

  def toOneOrMore: OneOrMore[A] = OneOrMore(head, (Iterator.single(_2) ++ more.iterator).toSeq)
}

object Several extends NonEmptyFactory[Several] {

  /** Factory to pass elements individually. */
  def apply[A](_1: A, _2: A, more: A*): Several[A] = Several(_1, _2, more)

  /** `Some[Several]` populated with the elements of `source` if `source` contains at least two elements, otherwise `None`.
    */
  def from[A](source: IterableOnce[A]): Option[Several[A]] = {
    val it = source.iterator
    if (it.hasNext) {
      val _1 = it.next()
      if (it.hasNext) {
        val _2 = it.next()
        Some(Several(_1, _2, it.toList))
      } else None
    } else None
  }

  /** Creates a `Several` populated with the elements of `source` in a non-safe way.
    * @throws IllegalArgumentException if `iterable` has less than two elements.
    */
  def fromUnsafe[A](source: IterableOnce[A]): Several[A] =
    from(source).getOrElse(
      throw new IllegalArgumentException(s"'source' of size ${source.iterator.size} passed to Several.fromUnsafe")
    )
}
