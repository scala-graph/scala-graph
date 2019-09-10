package scalax.time

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

/** Provides lightweight syntax for simple time measurement and the comparison of results.
  * Not aimed at sophisticated JVM benchmarking.
  *
  * Use `time`, `measure` or `measureAll` to get absolute times.
  * Use `relativeTime` or `relativeTimes` to get relative times.
  * Except `time`, all methods return `Result` that is comprised of the computation result(s) and elapsed times.
  * `warmUp` is the number of repeated computations prior to any measurement.
  * `repetitions` is the number of the repeated computation that will be measured.
  */
object MicroBenchmark {
  import scala.math.BigInt._
  import scala.math.Numeric

  implicit final class NanoSecond(val value: Long) extends AnyVal {
    def relativeTo(decimals: Int = 2)(that: NanoSecond): Float =
      round(this.value.toFloat / that.value.toFloat, decimals)
  }

  // allows to call functions requiring implicit Numeric such as sum
  implicit object NanoSecondNumeric extends Numeric[NanoSecond] {
    implicit def nanoSecondToLong(ns: NanoSecond): Long = ns.value
    val num                                             = Numeric.LongIsIntegral
    def plus(x: NanoSecond, y: NanoSecond)              = num.plus(x, y)
    def minus(x: NanoSecond, y: NanoSecond)             = num.minus(x, y)
    def times(x: NanoSecond, y: NanoSecond)             = num.times(x, y)
    def negate(x: NanoSecond)                           = num.negate(x)
    def fromInt(x: Int)                                 = x.toLong
    def toInt(x: NanoSecond)                            = x.value.toInt
    def toLong(x: NanoSecond)                           = x.value.toLong
    def toFloat(x: NanoSecond)                          = x.value.toFloat
    def toDouble(x: NanoSecond)                         = x.value.toDouble
    def compare(x: NanoSecond, y: NanoSecond)           = num.compare(x, y)
  }

  sealed abstract class MeasurementResult[A](result: A) {
    def mediumNanoSecs: Long
    def relativeTo(decimals: Int = 2)(that: MeasurementResult[A]) =
      this.mediumNanoSecs.relativeTo(decimals)(that.mediumNanoSecs)
    protected def toStringPrefix: String
    protected def optToStringParams = ""
    override def toString           = s"$toStringPrefix($mediumNanoSecs ns, $result$optToStringParams)"
  }

  case class SingleResult[A](nanoSecs: Long, result: A) extends MeasurementResult(result) {
    def mediumNanoSecs           = nanoSecs
    protected def toStringPrefix = "Result"
  }

  case class Result[A](result: A, times: ArrayBuffer[Long] = ArrayBuffer.empty) extends MeasurementResult(result) {
    def this(result: A, firstNanoSecs: Long) { this(result, ArrayBuffer(firstNanoSecs)) }
    def mediumNanoSecs: Long                 = times.sum / times.size
    def +=(nanoSecs: Long): this.type        = { this.times += nanoSecs; this }
    def nanoSecs                             = times.iterator
    protected def toStringPrefix             = "Results"
    override protected def optToStringParams = s""", {${nanoSecs mkString " "}}"""
  }

  case class Results[A](list: List[Result[A]]) {
    def relativeTimes(decimals: Int = 2): List[Float] = {
      val mA = list.head
      1f :: (list.tail map (r => r.relativeTo(decimals)(mA)))
    }
  }

  private def requireEq[A](r1: A, r2: A): Unit = require(
    r1 == r2,
    s"'$r1' != '$r2' but blocks are expected to return the same result. Otherwise set requireEqualResults to false."
  )

  def once[A](block: => A): SingleResult[A] = {
    val start = System.nanoTime
    val res   = block
    val end   = System.nanoTime
    SingleResult(end - start, res)
  }

  private def once[A](byName: ByName[A]): SingleResult[A] = once(byName())

  def measure[A](warmUp: Int = 1, repetitions: Int = 1)(block: => A): Result[A] =
    measureAll[A](warmUp, repetitions)(block).list.head

  def time[A](warmUp: Int = 1, repetitions: Int = 1)(block: => A): Long =
    measure(warmUp, repetitions)(block).mediumNanoSecs

  private def round(float: Float, decimals: Int) = {
    val fact = (10 pow decimals).toInt
    (float * fact).floor / fact
  }

  // relation of elapsed times b : a
  def relativeTime[A](warmUp: Int = 1, repetitions: Int = 1, decimals: Int = 2, requireEqualResults: Boolean = true)(
      a: => A,
      b: => A): Float =
    measureAll[A](warmUp, repetitions, requireEqualResults)(a, b).list match {
      case mA :: mB :: Nil => mB.relativeTo(decimals)(mA)
      case x               => throw new MatchError(x)
    }

  final class ByName[+A](x: => A) { def apply(): A = x }
  implicit def toHolder[A](block: => A): ByName[A] = new ByName(block)

  def measureAll[A](warmUp: Int = 1, repetitions: Int = 1, requireEqualResults: Boolean = true)(
      blocks: ByName[A]*): Results[A] = {
    require(repetitions > 0, "'repetitions' must be positive")
    for (i <- 1 to warmUp) blocks foreach once

    val results: Array[Result[A]] = (blocks map once map (r => new Result(r.result, r.nanoSecs))).toArray
    val zippedBlocks              = blocks.zipWithIndex
    for {
      i  <- 1 until repetitions
      zB <- zippedBlocks
    } {
      val (b, j) = zB
      once(b) match {
        case SingleResult(t, r) =>
          results(j) += t
          if (requireEqualResults) requireEq(r, results(j).result)
      }
    }
    Results(results.toList)
  }

  def relativeTimes[A](warmUp: Int = 1, repetitions: Int = 1, decimals: Int = 2, requireEqualResults: Boolean = true)(
      blocks: ByName[A]*): Seq[Float] =
    measureAll[A](warmUp, repetitions, requireEqualResults)(blocks: _*).relativeTimes(decimals)
}
