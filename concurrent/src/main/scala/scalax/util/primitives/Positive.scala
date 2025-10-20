package scalax.util.primitives

import scalax.util.primitives.NonNegative.IndexGen

/** 32 bit, verified, positive `Int` up to `Int.MaxValue - 8`.
 * The upper limit counts for maximum array size implementations. */
opaque type Positive = Int

object Positive extends Validated[Int, Positive]:
  private inline val lowerLimit = 1
  private inline val upperLimit = Int.MaxValue - 8

  protected inline def fromValid(a: Int): Positive = a
  protected inline def valid(a: Int): Boolean  = a >= lowerLimit && a <= upperLimit
  protected inline def errMsgSuffix: String    = " is not positive"

  extension (n: Positive)
    inline def toInt: Int = n
    private inline def asNonNegative: NonNegative = NonNegative.unsafe(n)

    /** `Iterator` over all `Int`s in { 0, ..., n - 1 }. */
    def indexIterator: Iterator[Int] = asNonNegative.indexIterator

    /** Calls `f` passing `Int`s in { 0, ..., n - 1 }. */
    inline def foreachIndex(f: Int => Unit): Unit =
      n.asNonNegative.foreachIndex(f)

    /** Generator of `Int`s in { 0, ..., n - 1 } with lazy materialization.
      * Use this as a safe replacement of `Range` that might cause `OutOfMemoryError` for a big `n`.
      */
    def gen: IndexGen = IndexGen(asNonNegative)

  given LimitedInt[Positive] with
    inline def lowerLimit: Positive = Positive.lowerLimit
    inline def upperLimit: Positive = Positive.upperLimit

    inline def lt(a: Positive, b: Positive): Boolean = a < b

    protected[scalax] inline def underlying(a: Positive): Int = a
    protected inline def fromValid(a: Int): Positive        = a

type PositiveSize = Positive
val PositiveSize = Positive