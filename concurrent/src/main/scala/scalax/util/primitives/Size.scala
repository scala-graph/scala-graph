package scalax.util.primitives

import scala.annotation.tailrec
import scala.collection.AbstractIterator

/** 32 bit, verified, positive `Int` up to `Int.MaxValue - 8` to support safe array sizes and more. */
opaque type Size = Int

object Size extends Validated[Int, Size]:
  private inline val lowerLimit = 1
  private inline val upperLimit = Int.MaxValue - 8

  protected inline def fromValid(a: Int): Size = a
  protected inline def valid(a: Int): Boolean  = a >= lowerLimit && a <= upperLimit
  protected inline def errMsgSuffix: String    = " is invalid for Size"

  extension (size: Size)
    inline def toInt: Int = size

    /** `Iterator` over all `Int`s in { 0, ..., size - 1 } with lazy materialization.
      */
    def indexIterator: Iterator[Int] =
      val limit = size
      new AbstractIterator[Int]:
        var i: Int = 0

        def hasNext: Boolean = i < limit
        def next(): Int      =
          val ret = i
          i += 1
          ret
        override def knownSize: Int = limit

    /** Calls `f` passing `Int`s in { 0, ..., size - 1 }.
      */
    def foreachIndex(f: Int => Unit): Unit =
      @tailrec def loop(i: Int): Unit =
        if i < size then {
          f(i)
          loop(i + 1)
        } else ()

      loop(0)

    /** Generator of `Int`s in { 0, ..., size - 1 } with lazy materialization.
      * Use this as a safe replacement of `Range` that might cause `OutOfMemoryError` for a big `Size`.
      */
    def gen: Gen = Gen(size)

  given LimitedInt[Size] with
    inline def lowerLimit: Size = Size.lowerLimit
    inline def upperLimit: Size = Size.upperLimit

    inline def lt(a: Size, b: Size): Boolean = a < b

    protected[scalax] inline def underlying(a: Size): Int = a
    protected inline def fromValid(a: Int): Size        = a

  final protected class Gen(limit: Size):
    inline def map[B](f: Int => B): Iterator[B]                   = iterator map f
    inline def flatMap[B](f: Int => IterableOnce[B]): Iterator[B] = iterator flatMap f
    inline def withFilter(p: Int => Boolean): Iterator[Int]       = iterator filter p

    inline def iterator: Iterator[Int]       = limit.indexIterator
    inline def foreach(f: Int => Unit): Unit = limit foreachIndex f
