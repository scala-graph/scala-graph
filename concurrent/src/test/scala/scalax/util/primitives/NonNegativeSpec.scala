package scalax.util.primitives

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class NonNegativeSpec extends RefSpec with Matchers with OptionValues:
  private inline val big   = 77_777_777
  private inline val limit = Int.MaxValue - 9
  private inline val above = Int.MaxValue

  def `apply `: Unit =
    "NonNegative(-1)" shouldNot compile
    NonNegative(0).toInt shouldBe 0
    NonNegative(1).toInt shouldBe 1
    NonNegative(big).toInt shouldBe big
    NonNegative(limit).toInt shouldBe limit
    "NonNegative(above)" shouldNot compile

  def `from `: Unit =
    NonNegative.from(-1) shouldBe empty
    NonNegative.from(0).value shouldBe 0
    NonNegative.from(1).value shouldBe 1
    NonNegative.from(big).value shouldBe big
    NonNegative.from(limit).value shouldBe limit
    NonNegative.from(above) shouldBe empty

  def `unsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(NonNegative.unsafe(-1))
    NonNegative.unsafe(0).toInt shouldBe 0
    NonNegative.unsafe(1).toInt shouldBe 1
    NonNegative.unsafe(big).toInt shouldBe big
    NonNegative.unsafe(limit).toInt shouldBe limit
    a[ValueOutOfBoundsException] shouldBe thrownBy(NonNegative.unsafe(above))

  def `< `: Unit =
    NonNegative(0) < NonNegative(0) shouldBe false
    NonNegative(1) < NonNegative(big) shouldBe true
    NonNegative(big) < NonNegative(limit) shouldBe true
    NonNegative(limit) < NonNegative(big) shouldBe false

  def `incr `: Unit =
    NonNegative(0).incr shouldBe NonNegative(1)
    NonNegative(big).incr shouldBe NonNegative(big + 1)
    a[LimitOverflowException.type] shouldBe thrownBy(NonNegative(limit).incr)

  def `+ `: Unit =
    NonNegative(0) + NonNegative(2) shouldBe NonNegative(2)
    a[LimitOverflowException.type] shouldBe thrownBy(NonNegative(1) + NonNegative(limit))
    NonNegative(big) + NonNegative(2) shouldBe NonNegative(big + 2)

  def `indexIterator `: Unit =
    Size(3).indexIterator.toList shouldBe List(0, 1, 2)

  def `foreachIndex `: Unit =
    var sum = 0
    Size(4).foreachIndex(sum += _)
    sum shouldBe 6

  def `gen `: Unit =
    (for i <- Size(3).gen yield i).toArray shouldBe Array(0, 1, 2)
