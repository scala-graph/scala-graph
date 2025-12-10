package scalax.util.primitives

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.compiletime.{codeOf, error}

/** 32 bit, verified, non-negative `Int` up to `Int.MaxValue - 9`.
  * The upper limit counts for maximum array size implementations.
  */
opaque type NonNegative = Int

object NonNegative extends Limited[Int, NonNegative], LimitedArithmetics[Int, NonNegative]:
  transparent inline def lowerLimit = 0
  transparent inline def upperLimit = Int.MaxValue - 9
  inline def zero: NonNegative      = 0

  inline def valid(a: Int): Boolean = a >= lowerLimit && a <= upperLimit

  final inline def apply(i: Int): NonNegative =
    inline if valid(i) then i
    else error(codeOf(i) + " is invalid for NonNegativeInt.")

  private inline given Limited[Int, NonNegative] = NonNegative
  extension (nn: NonNegative)
    inline def <(b: NonNegative): Boolean  = nn < b
    inline def >(b: NonNegative): Boolean  = nn > b
    inline def <=(b: NonNegative): Boolean = nn <= b
    inline def >=(b: NonNegative): Boolean = nn >= b

    inline def incr: NonNegative                   = LimitedIntImpl.incr(nn)
    inline def decr: NonNegative                   = LimitedIntImpl.decr(nn)
    inline def +(addend: NonNegative): NonNegative = LimitedIntImpl.addNonNegative(nn, addend)
    inline def *(factor: NonNegative): NonNegative = LimitedIntImpl.mulNonNegative(nn, factor)

    /** @throws ArithmeticException on division by zero. */
    inline def /(b: NonNegative): NonNegative = nn / b
    inline def %(b: NonNegative): NonNegative = nn % b

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
    infix def foreachIndex(f: Int => Unit): Unit =
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

type IntSize = NonNegative
val IntSize = NonNegative

type IntIndex = NonNegative
val IntIndex = NonNegative
