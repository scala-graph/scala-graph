package scalax.util.primitives

import scala.compiletime.{codeOf, error}

import scalax.util.primitives.NonNegativeInt.IndexGen

/** 32 bit, verified, positive `Int` up to `Int.MaxValue - 8`.
  * The upper limit counts for maximum array size implementations.
  */
opaque type Positive = Int

object Positive extends Limited[Int, Positive]:
  inline def lowerLimit: Int = 1
  inline def upperLimit: Int = Int.MaxValue - 8

  inline def valid(a: Int): Boolean =
    a >= lowerLimit && a <= upperLimit

  final inline def apply(i: Int): Positive =
    inline if valid(i) then i
    else error(codeOf(i) + " is invalid fpr Positive.")

  private inline given self: Limited[Int, Positive] = Positive
  extension (n: Positive)
    inline infix def <(b: Positive): Boolean = n < b

    inline def incr: Positive               = LimitedIntImpl.incr(n)
    infix def +(addend: Positive): Positive = LimitedIntImpl.add(n, addend)

    private inline def asNonNegative: NonNegativeInt = NonNegativeInt.trust(n)

    /** `Iterator` over all `Int`s in { 0, ..., n - 1 }. */
    def indexIterator: Iterator[Int] = asNonNegative.indexIterator

    /** Calls `f` passing `Int`s in { 0, ..., n - 1 }. */
    inline def foreachIndex(f: Int => Unit): Unit = asNonNegative.foreachIndex(f)

    /** Generator of `Int`s in { 0, ..., n - 1 } with lazy materialization.
      * Use this as a safe replacement of `Range` that might cause `OutOfMemoryError` for a big `n`.
      */
    def gen: IndexGen = IndexGen(asNonNegative)

type PositiveSize = Positive
val PositiveSize = Positive
