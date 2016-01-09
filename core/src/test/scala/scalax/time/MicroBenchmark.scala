package scalax.time

import scala.language.implicitConversions

/** Provides lightweight syntax for simple time measurement and the comparison of results.
 * Not aimed at sophisticated JVM benchmarking.
 */
object MicroBenchmark {
  import scala.math.BigInt._
  import scala.math.Numeric

  final case class NanoSecond(val self: Long) {
    override def toString = self.toString + " ns" 
    def relativeTo(decimals: Int = 2)(that: NanoSecond): Float =
      round(this.self.toFloat / that.self.toFloat, decimals)
  }
  implicit def longToNanoSecond(ns: Long): NanoSecond = NanoSecond(ns)
  implicit def nanoSecondToLong(ns: NanoSecond): Long = ns.self
  
  // allows to call functions requiring implicit Numeric such as sum 
  implicit object NanoSecondNumeric extends Numeric[NanoSecond] {
    val num = Numeric.LongIsIntegral
    def plus    (x: NanoSecond, y: NanoSecond) = num.plus(x,y)
    def minus   (x: NanoSecond, y: NanoSecond) = num.minus(x,y)
    def times   (x: NanoSecond, y: NanoSecond) = num.times(x,y)
    def negate  (x: NanoSecond) = num.negate(x)
    def fromInt (x: Int) = x
    def toInt   (x: NanoSecond) = x.self.toInt
    def toLong  (x: NanoSecond) = x.self.toLong
    def toFloat (x: NanoSecond) = x.self.toFloat
    def toDouble(x: NanoSecond) = x.self.toDouble
    def compare (x:NanoSecond, y:NanoSecond) = num.compare(x,y)
  }

  case class Result[A](nanoSecs: NanoSecond, result: A) {
    def relativeTo(decimals: Int = 2)(that: Result[A]) =
      this.nanoSecs.relativeTo(decimals)(that.nanoSecs)
  }

  def measure[A](warmUp: Int = 1,
                 repetitions: Int = 1)(block: => A): Result[A] = {
    def once = {
      val start = System.nanoTime
      val res = block
      val end = System.nanoTime
      Result(end - start, res)
    }
    for (i <- 1 to warmUp)
      block
    val Result(firstTime, result) = once
    var totalTime = firstTime
    for (i <- 2 to repetitions)
      once match {
        case Result(t, r) => totalTime += t
                             assert(r == result)
      }
    Result(totalTime / repetitions, result)
  }

  def time[A](warmUp: Int = 1,
              repetitions: Int = 1)(block: => A): Long =
    measure(warmUp, repetitions)(block).nanoSecs

  private def round(float: Float, decimals: Int) = { 
    val fact = (10 pow decimals).toInt
    (float * fact).floor / fact
  }
  
  // relation of elapsed times a : b
  def relativeTime[A](warmUp: Int = 1,
                      repetitions: Int = 1,
                      decimals: Int = 2)(a: => A, b: => A): Float = {
    val touch: Boolean = if (warmUp > 0) { a; b; true } else false
    val warmUpLeft = if (touch) warmUp - 1 else warmUp
    
    def m(x: => A): Result[A] = measure(warmUpLeft, repetitions)(x)
    val (mA, mB) = (m(a), m(b))
    assert(mA.result == mB.result)
    mA.relativeTo(decimals)(mB)
  }

  final class ByName[A](x: => A) { def apply(): A = x }
  implicit def toHolder[A](block: => A) = new ByName(block)

  def measureAll[A](warmUp: Int = 1,
                    repetitions: Int = 1)(
                    blocks: ByName[A]*): List[Result[A]] = {
    val touch: Boolean = if (warmUp > 0) { blocks foreach (_()); true} else false
    val warmUpLeft = if (touch) warmUp - 1 else warmUp
    val results =
      for( b <- blocks)
      yield measure(warmUpLeft, repetitions)(b())
    results.toList
  }
  
  def relativeTimes[A](results: List[Result[A]], decimals: Int) = {
    val mA = results.head
    1f :: (results.tail map { res =>
      assert(res.result == mA.result)
      res.relativeTo(decimals)(mA)
    })
  }
  
  def relativeTimes[A](warmUp: Int = 1,
                       repetitions: Int = 1,
                       decimals: Int = 2)(
                       blocks: ByName[A]*): List[Float] = {
    val results = measureAll[A](warmUp, repetitions)(blocks: _*)
    relativeTimes(results, decimals)
  }
}