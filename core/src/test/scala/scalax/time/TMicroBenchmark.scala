package scalax.time

import org.scalactic.Equality
import org.scalatest.Spec
import org.scalatest.Matchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TMicroBenchmarkTest
    extends Spec
       with Matchers {
  
  import MicroBenchmark._

  object `relativeTimes reflects` {
    def `relative execution times` {
      val r = 1 to 20
      val relTimes = relativeTimes(warmUp = 2)(
        r.toList.sorted, 
        r.toList.sorted.toArray.toList.sorted, 
        r.toList.sorted.toSet.toArray.toList.sorted 
      )
      relTimes sliding 2 foreach (_ match {
        case a :: b :: _ => a should be < b
        case x => throw new MatchError(x)
      })
    }
  }
  class FloatTolerance(maxDeviation: Float) extends Equality[Float] {
    private def eq(a: Float, b: Float): Boolean = if (a > b) a < b * maxDeviation else a > b / maxDeviation
    def areEqual(a: Float, b: Any) = b match {
      case f: Float => eq(a, f)
      case i: Int   => eq(a, i.toFloat)
    } 
  }
  object `relativeTime roughly reflects` {
    def `O(N) complexity of List.size` {
      def fill(size: Int): (Int, List[Int]) = (size, List.fill(size)(0)) 
      val (a, b) = (fill(100), fill(1000))
      
      implicit val tolerance = new FloatTolerance(2f)
      val expected = b._1.toFloat / a._1.toFloat
      val results = measureAll(warmUp = 5, repetitions = 10)(
          a._2.size == a._1,
          b._2.size == b._1)
      val actual = results.relativeTimes()(1)
      
      actual should === (expected)
    }
  }
  def `O(N) complexity of immutable Seq.size` {
    import scala.collection.mutable.ArrayBuffer
    val size = 1000
    val vector = Vector.fill(size)(0)
    val buffer = ArrayBuffer.fill(size)(0)

    implicit val tolerance = new FloatTolerance(1.8f)
    relativeTime()(buffer.size, vector.size) should === (size)
  }
  def `difference when traversing immutable vs. mutable Set` {
    import scala.collection.mutable
    val size = 10000
    val array = Array.tabulate(size)(identity)
    val sum = array.sum
    val imm = Set(array: _*)
    val m = mutable.Set(array: _*)

    relativeTime(repetitions = 6)(m.sum == sum, imm.sum == sum) should be > (1.5f)
  }
  def `difference when traversing immutable Set vs. mutable BitSet` {
    import scala.collection.mutable
    val size = 10000
    val array = Array.tabulate(size)(_ % (size / 10))
    val s = Set(array: _*)
    val b = mutable.BitSet(array: _*)

    relativeTime(warmUp = 20, repetitions = 6)(b.sum, s.sum) should be > (2f)
  }
}