package scalax.util.primitives

import scala.annotation.targetName
import scala.compiletime.{codeOf, constValue, error}
import scala.compiletime.ops.any.==
import scala.compiletime.ops.int.*

/** Represents power of 2 `Int` by its base-2 log.
  * The lowest valid `value` is `1`, which corresponds to the power of 2 number 2.
  * The highest valid `value` is `30`, which corresponds to the power of 2 number 1,073,741,824.
  */
opaque type PositiveLog2Value = Byte

object PositiveLog2Value extends Log2Value[PositiveLog2Value]:
  inline def lowerLimit: Byte = 1
  inline def upperLimit: Byte = 30

  private inline def notInValidRange  = " is not in the range of 1 to 30."
  private inline def notValidPowerOf2 = " is not a positive power of 2 with a positive log 2."

  inline def valid(byte: Byte): Boolean = byte >= lowerLimit && byte <= upperLimit

  inline def apply(byte: Byte): PositiveLog2Value =
    inline if valid(byte) then byte
    else error(codeOf(byte) + notInValidRange)

  inline def validLog2(byte: Int): Boolean  = byte >= lowerLimit && byte <= upperLimit
  inline def validPowerOf2(i: Int): Boolean = i > 1 && ((i & (i - 1)) == 0)

  inline def powerOf2[I <: Int & Singleton](i: I): PositiveLog2Value =
    type IsPowerOf2[I <: Int] <: Boolean = (I > 1) match
      case true  => BitwiseAnd[I, I - 1] == 0
      case false => false

    inline if constValue[IsPowerOf2[I]] then
      constValue[31 - NumberOfLeadingZeros[I]].toByte
    else error(codeOf(i) + notValidPowerOf2)

  inline def log2Unsafe(b: Byte): PositiveLog2Value =
    if validLog2(b) then b
    else throw ValueOutOfBoundsException(b, notInValidRange)

  /** @throws ValueOutOfBoundsException if `i` is not a positive power of 2 or equals to 1. */
  final def powerOf2Unsafe(i: Int): PositiveLog2Value =
    if validPowerOf2(i) then log2Unsafe(Integer.numberOfTrailingZeros(i).toByte)
    else throw ValueOutOfBoundsException(i, notValidPowerOf2)

  private inline given Log2Value[PositiveLog2Value] = PositiveLog2Value
  extension (k: PositiveLog2Value)
    /** The `value` formatted to make clear that it is a 2 exponent. */
    def show: String = s"2^${k.value}"

    /** The power of 2 `Int` that is represented by this `PositiveLog2Value`. */
    inline def asInt: Int = 1 << k

    /** The power of 2 `Positive` that is represented by this `PositiveLog2Value`. */
    inline def asPositive: Positive = Positive.trust(asInt)

    /** The power of 2 `NonNegative` that is represented by this `PositiveLog2Value`. */
    inline def asNonNegative: NonNegative = NonNegative.trust(asInt)

    inline def <(b: PositiveLog2Value): Boolean = k < b

    inline def incr: PositiveLog2Value        = LimitedByteImpl.incr(k)
    inline def incrTrusted: PositiveLog2Value = (k + 1).toByte
    inline def decr: PositiveLog2Value        = LimitedByteImpl.decr(k)
    inline def decrTrusted: PositiveLog2Value = (k - 1).toByte

    /** Multiplies 2^k^ by 2^l^ using bitwise `<<`.
      * @throws LimitOverflowException if the product exceeds `upperLimit`.
      */
    inline def *(l: PositiveLog2Value): PositiveLog2Value = Log2ValueImpl.mul(k, l)

object PositiveLog2ValueOverPositive:
  private inline given Limited[Int, Positive]       = Positive
  private inline given Log2Value[PositiveLog2Value] = PositiveLog2Value
  extension (n: Positive)
    /** Multiplies `n` by 2^k^ using bitwise `<<`.
      * @throws LimitOverflowException if the product exceeds `upperLimit`.
      */
    @targetName("mulPositiveByPositiveLog2")
    inline def *(k: PositiveLog2Value): Positive = Log2ValueImpl.mulO(n, k)

    /** Multiplies `n` by 2^k^ using bitwise `<<` without validating the product. */
    @targetName("mulPositiveByPositiveLog2Unchecked")
    inline def *!(k: PositiveLog2Value): Positive = Positive.trust(n.value << k.value)

    /** Divides `n` by 2^k^ using bitwise `>>`.
      * @throws LimitUnderflowException if the quotient is below `lowerLimit`.
      */
    @targetName("divPositiveByPositiveLog2")
    inline def /(k: PositiveLog2Value): Positive = Log2ValueImpl.divO(n, k)

    /** Divides `n` by 2^k^ using bitwise `>>` without validating the quotient. */
    @targetName("divPositiveByPositiveLog2Unchecked")
    inline infix def /!(k: PositiveLog2Value): Positive = Positive.trust(n.value >> k.value)

object PositiveLog2ValueOverNonNegative:
  private inline given Limited[Int, NonNegative]    = NonNegative
  private inline given Log2Value[PositiveLog2Value] = PositiveLog2Value
  extension (n: NonNegative)
    /** Multiplies `n` by 2^k^ using bitwise `<<`.
      * @throws LimitOverflowException if the product exceeds `upperLimit`.
      */
    @targetName("mulNonNegByPositiveLog2")
    inline def *(k: PositiveLog2Value): NonNegative = Log2ValueImpl.mulO(n, k)

    /** Multiplies `n` by 2^k^ using bitwise `<<` without validating the product. */
    @targetName("mulNonNegByPositiveLog2Unchecked")
    inline infix def *!(k: PositiveLog2Value): NonNegative =
      NonNegative.trust(n.value << k.value)

    /** Divides `n` by 2^k^ using bitwise `>>`. Validation is not necessary. */
    @targetName("divNonNegByPositiveLog2")
    inline def /(k: PositiveLog2Value): NonNegative =
      NonNegative.trust(n.value >> k.value)

    /** Returns the remainder of `n` divided by 2^k^ using bitwise `&`. Validation is not necessary. */
    @targetName("modNonNegByPositiveLog2")
    inline def %(k: PositiveLog2Value): NonNegative =
      n.mapTrusted(_ & (k.asInt - 1))
