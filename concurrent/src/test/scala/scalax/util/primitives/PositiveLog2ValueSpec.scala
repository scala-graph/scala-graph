package scalax.util.primitives

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class PositiveLog2ValueSpec extends RefSpec with Matchers:
  import PositiveLog2Value.*

  private inline val bigLog2   = 28
  private inline val limitLog2 = 30
  private inline val aboveLog2 = 31

  private inline val validPowerOf2   = 512
  private inline val invalidPowerOf2 = validPowerOf2 - 1
  private inline val abovePowerOf2   = 1 << aboveLog2

  def `apply `: Unit =
    "PositiveLog2Value(-1)" shouldNot compile
    "PositiveLog2Value(0)" shouldNot compile
    PositiveLog2Value(1).value shouldBe 1
    PositiveLog2Value(bigLog2).value shouldBe bigLog2
    PositiveLog2Value(limitLog2).value shouldBe limitLog2
    "PositiveLog2Value(above)" shouldNot compile

  def `powerOf2 `: Unit =
    "powerOf2(-1)" shouldNot compile
    "powerOf2(0)" shouldNot compile
    "powerOf2(1)" shouldNot compile
    powerOf2(2).value shouldBe 1
    powerOf2(64).value shouldBe 6
    "powerOf2(44)" shouldNot compile
    "powerOf2(Int.MaxValue)" shouldNot compile

  def `log2Unsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(log2Unsafe(0))
    log2Unsafe(1).value shouldBe 1
    log2Unsafe(bigLog2).value shouldBe bigLog2
    log2Unsafe(limitLog2).value shouldBe limitLog2
    a[ValueOutOfBoundsException] shouldBe thrownBy(log2Unsafe(aboveLog2))

  def `powerOf2Unsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(powerOf2Unsafe(1))
    powerOf2Unsafe(2).value shouldBe 1
    powerOf2Unsafe(2).asInt shouldBe 2
    powerOf2Unsafe(validPowerOf2).value shouldBe Integer.numberOfTrailingZeros(validPowerOf2)
    a[ValueOutOfBoundsException] shouldBe thrownBy(powerOf2Unsafe(invalidPowerOf2))
    a[ValueOutOfBoundsException] shouldBe thrownBy(powerOf2Unsafe(abovePowerOf2))

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
