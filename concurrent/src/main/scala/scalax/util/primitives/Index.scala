package scalax.util.primitives

/** 32 bit, verified, non-negative `Int` up to `Int.MaxValue - 9` to support safe array indexes and more. */
opaque type Index = Int

object Index extends Validated[Int, Index]:
  private inline val lowerLimit = 0
  private inline val upperLimit = Int.MaxValue - 9

  protected inline def fromValid(a: Int): Index = a
  protected inline def valid(a: Int): Boolean   = a >= lowerLimit && a <= upperLimit
  protected inline def errMsgSuffix: String     = " is invalid for Index"

  extension (index: Index) inline def toInt: Int = index

  given LimitedInt[Index] with
    inline def lowerLimit: Index = Index.lowerLimit
    inline def upperLimit: Index = Index.upperLimit

    inline def lt(a: Index, b: Index): Boolean = a < b

    protected[scalax] inline def underlying(a: Index): Int = a
    protected inline def fromValid(a: Int): Index          = a
