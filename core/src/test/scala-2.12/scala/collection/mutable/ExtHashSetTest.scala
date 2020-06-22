package scala.collection.mutable

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec

import scala.util.Random

class ExtHashSetTest extends RefSpec with Matchers {

  object `Hash set extensions work properly` {

    def `draw element` {
      val size = 32
      val r = new Random
      val set = ExtHashSet.fill(size)(r.nextInt(size * 2))
      val nrProbes = set.size * 10
      val probes = Array.fill(nrProbes)(set draw r)
      val lengths = probes.groupBy(identity).values.map(_.length)
      lengths.max - lengths.min should be < (nrProbes / 5)
    }

    def `find element by predicate` {
      case class C(value: Int) {
        override def hashCode(): Int = 0
      }
      val s = ExtHashSet(C(1), C(2))
      s.findElem(C(3), (i: C, j: C) => i.value == j.value - 1) should === (C(2))
    }

    def `upsert elements`: Unit = {
      class MutableElem(val a: Int, var b: Int) {
        override def hashCode(): Int = a.##
        override def equals(other: Any): Boolean = other match {
          case that: MutableElem => a == that.a
          case _                 => false
        }
        override def toString: String = s"M($a, $b)"
      }

      val elem = new MutableElem(1, 0)
      val set = ExtHashSet(elem)

      val mutation = new MutableElem(1, 1)
      mutation should ===(elem)

      set.upsert(mutation) should be(false)
      set should (have size 1 and contain(mutation))

      set.upsert(new MutableElem(2, 0)) should be(true)
      set should have size 2
    }

    def `iterate over hashCodes` {
      case class C(value: Int) {
        override def hashCode(): Int = value
      }
      val set = ExtHashSet(C(1), C(2))
      set.hashCodeIterator(3).toList should have size (0)
      val elems = set.hashCodeIterator(1).toList
      elems should have size (1)
      elems.head should be (C(1))
    }

    def `iterate over duplicate hashCodes` {
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
