package scala.collection.mutable

import scala.math.abs
import scala.util.chaining._

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec
import scala.util.Random

class ExtHashSetTest extends RefSpec with Matchers {

  object `ExtHashSet works properly in that it` {
    import ExtHashSetTest._

    def `draws random elements with uniform distribution if buckets are of equal length` {
      val size = 16
      val set  = ExtHashSet(1 to size: _*)
      nonEmptyBuckets(set).length should ===(size)

      val nrProbesPerElement = 100
      val nrProbes           = size * nrProbesPerElement
      val frequencies        = sortedProbeFrequencies(set, nrProbes, Some(0))
      val maxDeviation_%     = 20
      range(frequencies) should be < (nrProbesPerElement * (2 * maxDeviation_%) / 100)
    }

    def `draws random elements with near-uniform distribution if buckets are of different length` {
      val set = {
        val maxSize = 16
        val r       = new Random
        r.setSeed(1150)
        ExtHashSet.fill(maxSize)(r.nextInt(maxSize * 4))
      }

      val buckets = nonEmptyBuckets(set)
      buckets.map(_.length) pipe { bucketLengths =>
        bucketLengths.max - bucketLengths.min should be > 0
      }

      val bucketLengthOfElement: Int Map Int = {
        val m = Map.empty[Int, Int]
        buckets foreach { bucket =>
          val len = bucket.length
          bucket foreach (v => m update (v, len))
        }
        m
      }
      val nrProbesPerElement            = 100
      val nrProbes                      = set.size * nrProbesPerElement
      val frequencies                   = sortedProbeFrequencies(set, nrProbes, Some(77))
      val maxDeviationForBucketLength_% = Map(1 -> 25, 2 -> 50)
      frequencies foreach {
        case ProbeFrequency(element, count) =>
          abs(nrProbesPerElement - count) should be < maxDeviationForBucketLength_%(bucketLengthOfElement(element))
      }
    }

    def `finds elements by predicate` {
      case class C(value: Int) {
        override def hashCode(): Int = 0
      }
      val s = ExtHashSet(C(1), C(2))
      s.findElem(C(3), (i: C, j: C) => i.value == j.value - 1) should ===(C(2))
    }

    def `is able to upsert elements`: Unit = {
      class MutableElem(val a: Int, var b: Int) {
        override def hashCode(): Int = a.##
        override def equals(other: Any): Boolean = other match {
          case that: MutableElem => a == that.a
          case _                 => false
        }
        override def toString: String = s"M($a, $b)"
      }

      val elem = new MutableElem(1, 0)
      val set  = ExtHashSet(elem)

      val mutation = new MutableElem(1, 1)
      mutation should ===(elem)

      set.upsert(mutation) should be(false)
      set should (have size 1 and contain(mutation))

      set.upsert(new MutableElem(2, 0)) should be(true)
      set should have size 2
    }

    def `iterates over hashCodes` {
      case class C(value: Int) {
        override def hashCode(): Int = value
      }
      val set = ExtHashSet(C(1), C(2))
      set.hashCodeIterator(3).toList should have size (0)
      val elems = set.hashCodeIterator(1).toList
      elems should have size (1)
      elems.head should be(C(1))
    }

    def `iterates over duplicate hashCodes` {
      case class C(i: Int, j: Int) {
        override def hashCode = i.##
      }
      val multi = ExtHashSet(C(1, 0), C(1, 1), C(2, 2), C(1, 3), C(2, 0))
      for (i <- 0 to 2) {
        val elems = multi.hashCodeIterator(i.##).toList
        elems should have size (multi count (_.i == i))
      }
    }
  }
}

object ExtHashSetTest extends App {

  private def nonEmptyBuckets(set: ExtHashSet[Int]) = set.dump filter (_.nonEmpty)

  protected case class ProbeFrequency[A](probe: A, count: Int)

  private def sortedProbeFrequencies(set: ExtHashSet[Int],
                                     nrProbes: Int,
                                     seed: Option[Long]): List[ProbeFrequency[Int]] = {
    val r = new Random
    seed foreach r.setSeed
    Array
      .fill(nrProbes)(set draw r)
      .groupBy(identity)
      .map {
        case (k, v: Array[Int]) => ProbeFrequency(k, v.length)
      }
      .toList
      .sortBy {
        case ProbeFrequency(_, v) => v
      }
  }

  private def range(sortedFrequencies: List[ProbeFrequency[Int]]): Int =
    (sortedFrequencies.head, sortedFrequencies.last) match {
      case (ProbeFrequency(_, lowest), ProbeFrequency(_, highest)) => highest - lowest
    }

  /* some support to play on the console:

  val set = ExtHashSet(0, 1, 7, 10, 12, 81, 82, 83)
  set.dump foreach println

  Range(10, 2000, 100) foreach { i =>
    println(sortedProbeFrequencies(set, i))
  }
 */
}
