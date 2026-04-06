package scalax.util.primitives

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class NonNegativeSpec extends RefSpec, Matchers, OptionValues:
  private inline val big   = 77_777_777
  private inline val limit = Int.MaxValue - 9
  private inline val above = Int.MaxValue

  def `apply `: Unit =
    "NonNegative(-1)" shouldNot compile
    NonNegative(0).value shouldBe 0
    NonNegative(1).value shouldBe 1
    NonNegative(big).value shouldBe big
    NonNegative(limit).value shouldBe limit
    "NonNegative(above)" shouldNot compile

  def `from `: Unit =
    NonNegative.from(-1) shouldBe empty
    NonNegative.from(0).value.value shouldBe 0
    NonNegative.from(1).value.value shouldBe 1
    NonNegative.from(big).value.value shouldBe big
    NonNegative.from(limit).value.value shouldBe limit
    NonNegative.from(above) shouldBe empty

  def `unsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(NonNegative.unsafe(-1))
    NonNegative.unsafe(0).value shouldBe 0
    NonNegative.unsafe(1).value shouldBe 1
    NonNegative.unsafe(big).value shouldBe big
    NonNegative.unsafe(limit).value shouldBe limit
    a[ValueOutOfBoundsException] shouldBe thrownBy(NonNegative.unsafe(above))

  def `trust `: Unit =
    Positive.trust(-1).value shouldBe -1
    Positive.trust(1).value shouldBe 1

  def `< `: Unit =
    NonNegative(0) < NonNegative(0) shouldBe false
    NonNegative(1) < NonNegative(big) shouldBe true
    NonNegative(big) < NonNegative(limit) shouldBe true
    NonNegative(limit) < NonNegative(big) shouldBe false

  def `incr `: Unit =
    NonNegative(0).incr shouldBe NonNegative(1)
    NonNegative(big).incr shouldBe NonNegative(big + 1)
    a[LimitOverflowException] shouldBe thrownBy(NonNegative(limit).incr)

  def `+ `: Unit =
    NonNegative(0) + NonNegative(2) shouldBe NonNegative(2)
    NonNegative(2) + NonNegative(0) shouldBe NonNegative(2)
    a[LimitOverflowException] shouldBe thrownBy(NonNegative(1) + NonNegative(limit))
    NonNegative(big) + NonNegative(2) shouldBe NonNegative(big + 2)

  def `* `: Unit =
    NonNegative(0) * NonNegative(2) shouldBe NonNegative(0)
    NonNegative(2) * NonNegative(0) shouldBe NonNegative(0)
    a[LimitOverflowException] shouldBe thrownBy(NonNegative.trust(limit / 2) * NonNegative(3))
    NonNegative(big) * NonNegative(2) shouldBe NonNegative(big * 2)

  def `mapValidated `: Unit =
    NonNegative(0).mapValidated(_ + 3).value shouldBe NonNegative(3)
    NonNegative(0).mapValidated(_ => -3) shouldBe empty

  def `indexIterator `: Unit =
    Size(3).indexes.toList shouldBe List(0, 1, 2)

  def `foreachIndex `: Unit =
    var sum = 0
    Size(4).foreachIndex(sum += _)
    sum shouldBe 6

  def `gen `: Unit =
    (for i <- Size(3).gen yield i).toArray shouldBe Array(0, 1, 2)

  def `unused `: Unit =
    NonNegative.unused1 should be > NonNegative.upperLimit
    NonNegative.unused2 should be > NonNegative.upperLimit
