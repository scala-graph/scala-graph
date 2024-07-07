package scalax.time

import org.scalactic.Equality
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/* Intentionally excluded from SBT tests by means of a class name suffix different from "Spec".
 */
class MicroBenchmarkTest extends RefSpec with Matchers {

  import MicroBenchmark.*

  object `relativeTimes() reflects` {
    def `relative execution times`: Unit = {
      val r = 1 to 20
      val relTimes = relativeTimes(warmUp = 2)(
        r.toList.sorted,
        r.toList.sorted.toArray.toList.sorted,
        r.toList.sorted.toSet.toArray.toList.sorted
      )
      relTimes sliding 2 foreach {
        case a :: b :: _ => a should be < b
        case x           => throw new MatchError(x)
      }
    }
  }

  class FloatTolerance(maxDeviation: Float) extends Equality[Float] {
    private def eq(a: Float, b: Float): Boolean = if (a > b) a < b * maxDeviation else a > b / maxDeviation
    def areEqual(a: Float, b: Any) = b match {
      case f: Float => eq(a, f)
      case i: Int   => eq(a, i.toFloat)
    }
  }

  object `relativeTime() roughly reflects` {
    def `O(N) complexity of List.size`: Unit = {
      def fill(size: Int): (Int, List[Int]) = (size, List.fill(size)(0))
      val (small, big)                      = (fill(100), fill(1000))

      implicit val tolerance = new FloatTolerance(4f)
      val expected           = big._1.toFloat / small._1.toFloat
      val results = measureAll(warmUp = 5, repetitions = 10)(small._2.size == small._1, big._2.size == big._1)
      val actual  = results.relativeTimes()(1)

      actual should ===(expected)
    }
  }

  def `traversing immutable.Set takes marginally longer than mutable.Set`: Unit = {
    val size = 10000
    val seq  = ArraySeq.tabulate(size)(identity)
    val sum  = seq.sum
    val imm  = Set(seq: _*)
    val m    = mutable.Set(seq: _*)

    relativeTime(repetitions = 6)(m.sum == sum, imm.sum == sum) should be > 1.05f
  }

  /* assumption not correct
  def `traversing mutable.Set takes longer than mutable.BitSet`: Unit = {
    val size  = 10000
    val seq = ArraySeq.tabulate(size)(_ % (size / 10))
    val s     = Set(seq: _*)
    val b     = mutable.BitSet(seq: _*)

    relativeTime(warmUp = 20, repetitions = 6)(b.sum, s.sum) should be > 1.1f
  }
   */
}
