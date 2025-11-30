package scalax.util.primitives

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class NonNegativeIntSpec extends RefSpec, Matchers, OptionValues:
  private inline val big   = 77_777_777
  private inline val limit = Int.MaxValue - 9
  private inline val above = Int.MaxValue

  def `apply `: Unit =
    "NonNegative(-1)" shouldNot compile
    NonNegativeInt(0).value shouldBe 0
    NonNegativeInt(1).value shouldBe 1
    NonNegativeInt(big).value shouldBe big
    NonNegativeInt(limit).value shouldBe limit
    "NonNegative(above)" shouldNot compile

  def `from `: Unit =
    NonNegativeInt.from(-1) shouldBe empty
    NonNegativeInt.from(0).value.value shouldBe 0
    NonNegativeInt.from(1).value.value shouldBe 1
    NonNegativeInt.from(big).value.value shouldBe big
    NonNegativeInt.from(limit).value.value shouldBe limit
    NonNegativeInt.from(above) shouldBe empty

  def `unsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(NonNegativeInt.unsafe(-1))
    NonNegativeInt.unsafe(0).value shouldBe 0
    NonNegativeInt.unsafe(1).value shouldBe 1
    NonNegativeInt.unsafe(big).value shouldBe big
    NonNegativeInt.unsafe(limit).value shouldBe limit
    a[ValueOutOfBoundsException] shouldBe thrownBy(NonNegativeInt.unsafe(above))

  def `trust `: Unit =
    Positive.trust(-1).value shouldBe -1
    Positive.trust(1).value shouldBe 1

  def `< `: Unit =
    NonNegativeInt(0) < NonNegativeInt(0) shouldBe false
    NonNegativeInt(1) < NonNegativeInt(big) shouldBe true
    NonNegativeInt(big) < NonNegativeInt(limit) shouldBe true
    NonNegativeInt(limit) < NonNegativeInt(big) shouldBe false

  def `incr `: Unit =
    NonNegativeInt(0).incr shouldBe NonNegativeInt(1)
    NonNegativeInt(big).incr shouldBe NonNegativeInt(big + 1)
    a[LimitOverflowException.type] shouldBe thrownBy(NonNegativeInt(limit).incr)

  def `+ `: Unit =
    NonNegativeInt(0) + NonNegativeInt(2) shouldBe NonNegativeInt(2)
    a[LimitOverflowException.type] shouldBe thrownBy(NonNegativeInt(1) + NonNegativeInt(limit))
    NonNegativeInt(big) + NonNegativeInt(2) shouldBe NonNegativeInt(big + 2)

  def `indexIterator `: Unit =
    IntSize(3).indexIterator.toList shouldBe List(0, 1, 2)

  def `foreachIndex `: Unit =
    var sum = 0
    IntSize(4).foreachIndex(sum += _)
    sum shouldBe 6

  def `gen `: Unit =
    (for i <- IntSize(3).gen yield i).toArray shouldBe Array(0, 1, 2)
