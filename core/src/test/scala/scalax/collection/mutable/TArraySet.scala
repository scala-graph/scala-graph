package scalax.collection
package mutable

import collection.mutable.{Set => MutableSet}

import edge.LkDiEdge, edge.WUnDiEdge, edge.Implicits._
import immutable.SortedArraySet

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TArraySetTest extends RefSpec with Matchers {

  implicit val hints = ArraySet.Hints(4, 4, 12, 100)

  private class LkDiEdgeGenerator {
    private var i           = 0
    def draw: LkDiEdge[Int] = { i += 1; LkDiEdge(1, 2)(i) }
  }

  private class WUnDiEdgeGenerator {
    private var i            = 0
    def draw: WUnDiEdge[Int] = { i += 1; WUnDiEdge(i, i + 1)(2 * i + 1) }
  }

  object `ArraySet ` {
    def `can grow` {
      val arr = ArraySet.emptyWithHints[LkDiEdge[Int]]
      arr.capacity should be(hints.initialCapacity)

      val edges = new LkDiEdgeGenerator
      def add(numberOfAdditions: Int, expectedCapacity: Int) {
        for (i <- 0 until numberOfAdditions) {
          arr += edges.draw
          arr.capacity should be(expectedCapacity)
        }
      }

      var toAdd, nextCapacity = hints.initialCapacity
      while (nextCapacity <= hints.hashTableThreshold) {
        add(toAdd, nextCapacity)
        nextCapacity += hints.capacityIncrement
        toAdd = hints.capacityIncrement
      }
      add(1, 0)
      arr.isArray should be(false)
    }

    def `may be compacted` {
      val edges = new LkDiEdgeGenerator
      val toAdd = hints.initialCapacity + 1
      val arr = ArraySet.emptyWithHints[LkDiEdge[Int]] ++=
        (for (i <- 1 to toAdd) yield edges.draw)
      arr.compact
      arr.capacity should be(toAdd)

    }
    def `may be configured to be represented solely by a HashSet` {
      val edges = new LkDiEdgeGenerator
      val arr   = ArraySet.emptyWithHints[LkDiEdge[Int]](ArraySet.Hints.HashOnly)
      def check {
        arr.isArray should be(false)
        arr.capacity should be(0)
      }
      check

      arr += edges.draw
      check

      arr.compact
      check
    }

    def `supports hints` {
      val edges = new LkDiEdgeGenerator
      val arr   = ArraySet.emptyWithHints[LkDiEdge[Int]](ArraySet.Hints(0, 4, 8, 0))
      arr += edges.draw
      arr.capacity should be(4)
    }

    def `supports hints properly when filtered` {
      val edges = new LkDiEdgeGenerator
      type E = LkDiEdge[Int]
      val arr  = ArraySet.emptyWithHints[E]
      val size = hints.initialCapacity + 1
      for (i <- 1 to size) arr += edges.draw

      val taken = arr take size
      taken.isInstanceOf[ArraySet[_]] should be(true)
      taken should have size (size)

      def setInterface[A](set: MutableSet[A], n: Int): MutableSet[A] = set take n
      setInterface(arr, size - 1).isInstanceOf[ArraySet[_]] should be(true)

      val filtered0 = arr filter (_ => false)
      filtered0.isInstanceOf[ArraySet[_]] should be(true)
      taken should have size (size)
      filtered0.hints.initialCapacity should equal(arr.size)

      for (i <- 1 to hints.capacityIncrement) arr += edges.draw
      val filteredEven = arr filter (_ % 2 == 0)
      filteredEven.hints.initialCapacity should equal(arr.size)
    }

    def `is sortable` {
      val as  = ArraySet(3, 6, 0, -3)
      val sas = as.sorted
      sas.isInstanceOf[SortedArraySet[_]] should be(true)
      sas.filter(_ < 0).isInstanceOf[SortedArraySet[_]] should be(true)
      sas.toList should be(List(-3, 0, 3, 6))
      sas.from(1) should be(SortedArraySet(3, 6))
      sas.until(0) should be(SortedArraySet(-3))
      sas.range(-10, 10) should be(sas)
      sas.range(-10, 1) should be(SortedArraySet(-3, 0))
      sas.range(-10, -3) should be(SortedArraySet.empty[Int])
      sas.range(-10, -4) should be(SortedArraySet.empty[Int])
    }

    def `supports ++` {
      val a = ArraySet.empty[Int]
      val b = ArraySet(1)
      val c = ArraySet(2)

      a.clone should be('isEmpty)
      a ++ b should be(b)
      b ++ c should be(b.toSet ++ c.toSet)
    }

    object `supports upsert` {
      def upsert(toAdd: Int) {
        val edges = new WUnDiEdgeGenerator
        val pos   = 1
        pos < toAdd should be(true)

        val arr = ArraySet.emptyWithHints[WUnDiEdge[Int]] ++=
          (for (i <- 1 to toAdd) yield edges.draw)
        arr.size should be(toAdd)

        def edge = arr.drop(pos).head
        edge match {
          case WUnDiEdge(n1, n2, w) =>
            val newWeight = w + 1
            val res       = arr.upsert(WUnDiEdge(n1, n2)(newWeight))
            res should be(false) // updated
            edge.weight should be(newWeight)
        }
        arr.size should be(toAdd)
        (arr upsert edges.draw) should be(true) // inserted
        arr.size should be(toAdd + 1)
      }
      def `when represented by an Array` {
        upsert(hints.hashTableThreshold - 3)
      }
      def `when represented by a HashSet` {
        upsert(hints.hashTableThreshold + 3)
      }
    }
  }
}
