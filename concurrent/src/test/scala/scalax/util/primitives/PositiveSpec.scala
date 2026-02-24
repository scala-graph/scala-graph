package scalax.util.primitives

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class PositiveSpec extends RefSpec with Matchers with OptionValues:
  private inline val big   = 77_777_777
  private inline val limit = Int.MaxValue - 8
  private inline val above = Int.MaxValue

  def `apply `: Unit =
    "Positive(-1)" shouldNot compile
    "Positive(0)" shouldNot compile
    Positive(1).value shouldBe 1
    Positive(big).value shouldBe big
    Positive(limit).value shouldBe limit
    "Positive(above)" shouldNot compile

  def `from `: Unit =
    Positive.from(-1) shouldBe empty
    Positive.from(0) shouldBe empty
    Positive.from(1).value.value shouldBe 1
    Positive.from(big).value.value shouldBe big
    Positive.from(limit).value.value shouldBe limit
    Positive.from(above) shouldBe empty

  def `unsafe `: Unit =
    a[ValueOutOfBoundsException] shouldBe thrownBy(Positive.unsafe(-1))
    a[ValueOutOfBoundsException] shouldBe thrownBy(Positive.unsafe(0))
    Positive.unsafe(1).value shouldBe 1
    Positive.unsafe(big).value shouldBe big
    Positive.unsafe(limit).value shouldBe limit
    a[ValueOutOfBoundsException] shouldBe thrownBy(Positive.unsafe(above))

  def `trust `: Unit =
    Positive.trust(-1).value shouldBe -1
    Positive.trust(1).value shouldBe 1

  def `< `: Unit =
    Positive(1) < Positive(1) shouldBe false
    Positive(1) < Positive(big) shouldBe true
    Positive(big) < Positive(limit) shouldBe true
    Positive(limit) < Positive(big) shouldBe false

  def `incr `: Unit =
    Positive(1).incr shouldBe Positive(2)
    Positive(big).incr shouldBe Positive(big + 1)
    a[LimitOverflowException] shouldBe thrownBy(Positive(limit).incr)

  def `+ `: Unit =
    Positive(1) + Positive(2) shouldBe Positive(3)
    a[LimitOverflowException] shouldBe thrownBy(Positive(1) + Positive(limit))
    Positive(big) + Positive(2) shouldBe Positive(big + 2)

  def `* `: Unit =
    Positive(1) * Positive(2) shouldBe Positive(2)
    Positive(2) * Positive(1) shouldBe Positive(2)
    a[LimitOverflowException] shouldBe thrownBy(Positive.trust(limit / 2) * Positive(3))
    Positive(big) * Positive(2) shouldBe Positive(big * 2)

  def `/ `: Unit =
    Positive(1) / Positive(1) shouldBe Positive(1)
    Positive(2) / Positive(1) shouldBe Positive(2)
    a[LimitUnderflowException] shouldBe thrownBy(Positive(1) / Positive(2))
    Positive(big) / Positive(2) shouldBe Positive(big / 2)

  def `mapValidated `: Unit =
    Positive(1).mapValidated(_ + 3).value shouldBe Positive(4)
    Positive(1).mapValidated(_ => -3) shouldBe empty

  def `indexIterator `: Unit =
    PositiveSize(3).indexes.toList shouldBe List(0, 1, 2)

  def `foreachIndex `: Unit =
    var sum = 0
    PositiveSize(4).foreachIndex(sum += _)
    sum shouldBe 6

  def `gen `: Unit =
    (for i <- PositiveSize(3).gen yield i).toArray shouldBe Array(0, 1, 2)
