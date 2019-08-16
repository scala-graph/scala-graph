package scala.collection
package mutable

import scala.util.Random

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec

class ExtHashSetTest extends RefSpec with Matchers {

  object `Hash set extensions work properly` {

    def `draw element` {
      val size = 32
      val r = new Random
      val set = mutable.ExtHashSet.fill(size)(r.nextInt(size * 2))
      val nrProbes = set.size * 10
      val probes = Array.fill(nrProbes)(set draw r)
      val lengths = probes.groupBy(identity).values.map(_.length)
      lengths.max - lengths.min should be < (nrProbes / 5)
    }

    def `find element by predicate` {
      case class C(value: Int) {
        override def hashCode(): Int = 0
      }
      val s = mutable.ExtHashSet(C(1), C(2))
      s.findElem(C(3), (i: C, j: C) => i.value == j.value - 1) should === (C(2))
    }

    def `iterate over hashCodes` {
      set.hashCodeIterator(-228876066).toList should have size (0)
      outerEdge.hashCode should be(innerEdge.hashCode)
      val elems = set.hashCodeIterator(outerEdge.hashCode).toList
      elems should have size (1)
      elems.head should be(outerEdge)
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