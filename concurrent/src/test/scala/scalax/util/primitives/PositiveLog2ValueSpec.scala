package scalax.util.primitives

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class PositiveLog2ValueSpec extends RefSpec with Matchers:
  import PositiveLog2Value.*

  private inline val validLog2 = 9
  private inline val bigLog2   = 28
  private inline val limitLog2 = 30
  private inline val aboveLog2 = 31

  private inline val validPowerOf2   = 1 << validLog2
  private inline val invalidPowerOf2 = validPowerOf2 - 1
  private inline val maxInt          = 1 << limitLog2
  private inline val abovePowerOf2   = 1 << aboveLog2

  def `apply `: Unit =
    "PositiveLog2Value(-1)" shouldNot compile
    "PositiveLog2Value(0)" shouldNot compile
    PositiveLog2Value(1).value shouldBe 1
    PositiveLog2Value(bigLog2).value shouldBe bigLog2
    PositiveLog2Value(limitLog2).value shouldBe limitLog2
    "PositiveLog2Value(above)" shouldNot compile

  def `fromPowerOf2 `: Unit =
    "fromPowerOf2(-1)" shouldNot compile
    "fromPowerOf2(0)" shouldNot compile
    "fromPowerOf2(1)" shouldNot compile
    fromPowerOf2(2).value shouldBe 1
    fromPowerOf2(64).value shouldBe 6
    "fromPowerOf2(44)" shouldNot compile
    "fromPowerOf2(Int.MaxValue)" shouldNot compile

  def `fromPowerOf2Unsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(fromPowerOf2Unsafe(1))
    fromPowerOf2Unsafe(2).value shouldBe 1
    fromPowerOf2Unsafe(2).asInt shouldBe 2
    fromPowerOf2Unsafe(validPowerOf2).value shouldBe Integer.numberOfTrailingZeros(validPowerOf2)
    a[ValueOutOfBoundsException] shouldBe thrownBy(fromPowerOf2Unsafe(invalidPowerOf2))
    a[ValueOutOfBoundsException] shouldBe thrownBy(fromPowerOf2Unsafe(abovePowerOf2))

  def `fromExponentUnsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(fromExponentUnsafe(0))
    fromExponentUnsafe(1).value shouldBe 1
    fromExponentUnsafe(bigLog2).value shouldBe bigLog2
    fromExponentUnsafe(limitLog2).value shouldBe limitLog2
    a[ValueOutOfBoundsException] shouldBe thrownBy(fromExponentUnsafe(aboveLog2))

  def `ceilLog2 `: Unit =
    "ceilLog2(0)" shouldNot compile
    "ceilLog2(1)" shouldNot compile
    ceilLog2(2).value shouldBe 1
    ceilLog2(invalidPowerOf2).value shouldBe validLog2
    ceilLog2(maxInt - 1).value shouldBe limitLog2
    ceilLog2(maxInt).value shouldBe limitLog2
    "ceilLog2(abovePowerOf2)" shouldNot compile

  def `ceilLog2Unsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(ceilLog2Unsafe(0))
    a[ValueOutOfBoundsException] shouldBe thrownBy(ceilLog2Unsafe(1))
    ceilLog2Unsafe(2).value shouldBe 1
    ceilLog2Unsafe(invalidPowerOf2).value shouldBe validLog2
    ceilLog2Unsafe(maxInt - 1).value shouldBe limitLog2
    ceilLog2Unsafe(maxInt).value shouldBe limitLog2
    a[ValueOutOfBoundsException] shouldBe thrownBy(ceilLog2Unsafe(abovePowerOf2))

  def `asInt `: Unit =
    PositiveLog2Value(bigLog2).asInt shouldBe 1 << bigLog2

  def `asPositive `: Unit =
    PositiveLog2Value(1).asPositive shouldBe Positive(2)

  def `* `: Unit =
    PositiveLog2Value(4) * PositiveLog2Value(2) shouldBe PositiveLog2Value(6)

    import PositiveLog2ValueOverPositive.`*`
    import PositiveLog2ValueOverNonNegative.`*`

    Positive(3) * PositiveLog2Value(1) shouldBe Positive(6)
    NonNegative(3) * PositiveLog2Value(1) shouldBe NonNegative(6)
    a[LimitOverflowException] shouldBe thrownBy(NonNegative.unsafe(Int.MaxValue - 10) * PositiveLog2Value(1))

  def `/ `: Unit =
    import PositiveLog2ValueOverPositive.`/`
    import PositiveLog2ValueOverNonNegative.`/`

    Positive(3) / PositiveLog2Value(1) shouldBe Positive(1)
    NonNegative(10) / PositiveLog2Value(2) shouldBe NonNegative(2)
    a[LimitUnderflowException] shouldBe thrownBy(Positive(3) / PositiveLog2Value(2))

  def `% `: Unit =
    import PositiveLog2ValueOverNonNegative.`%`

    NonNegative(11) % PositiveLog2Value(2) shouldBe NonNegative(3)
    NonNegative(0)  % PositiveLog2Value(2) shouldBe NonNegative(0)
