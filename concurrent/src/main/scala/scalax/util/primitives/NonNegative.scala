package scalax.util.primitives

import scala.annotation.tailrec
import scala.collection.AbstractIterator

/** 32 bit, verified, non-negative `Int` up to `Int.MaxValue - 9`.
  * The upper limit counts for maximum array size implementations.
  */
opaque type NonNegative = Int

object NonNegative extends LimitedInt[NonNegative]:
  inline val lowerLimit = 0
  inline val upperLimit = Int.MaxValue - 9

  protected inline def valid(a: Int): Boolean = a >= lowerLimit && a <= upperLimit
  protected inline def errMsgSuffix: String   = " is invalid for Index"

  extension (nn: NonNegative)
    /** `Iterator` over all `Int`s in { 0, ..., n - 1 }. */
    def indexIterator: Iterator[Int] =
      new AbstractIterator[Int]:
        var i: Int = 0

        def hasNext: Boolean = i < nn
        def next(): Int      =
          val ret = i
          i += 1
          ret
        override def knownSize: Int = nn

    /** Calls `f` passing `Int`s in { 0, ..., n - 1 }. */
    def foreachIndex(f: Int => Unit): Unit =
      @tailrec def loop(i: Int): Unit =
        if i < nn then {
          f(i)
          loop(i + 1)
        } else ()
      loop(0)

    /** Generator of `Int`s in { 0, ..., n - 1 } with lazy materialization.
      * Use this as a safe replacement of `Range` that might cause `OutOfMemoryError` for a big `n`.
      */
    def gen: IndexGen = IndexGen(nn)

  final protected[primitives] class IndexGen(limit: NonNegative):
    inline def map[B](f: Int => B): Iterator[B]                   = iterator map f
    inline def flatMap[B](f: Int => IterableOnce[B]): Iterator[B] = iterator flatMap f
    inline def withFilter(p: Int => Boolean): Iterator[Int]       = iterator filter p
    inline def iterator: Iterator[Int]                            = limit.indexIterator
    inline def foreach(f: Int => Unit): Unit                      = limit foreachIndex f

//  given Conversion[NonNegative, Int] = (nn: NonNegative) => nn

type Size = NonNegative
val Size = NonNegative

type Index = NonNegative
val Index = NonNegative
