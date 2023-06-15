package scalax.collection.mutable

import scala.collection.mutable.{Set => MutableSet}
import scala.util.chaining._

import scalax.collection.immutable.SortedArraySet

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class ArraySetSpec extends RefSpec with Matchers {

  implicit val hints: ArraySet.Hints = ArraySet.Hints(
    initialCapacity = 4,
    capacityIncrement = 4,
    hashTableThreshold = 12,
    compactUpToUsed = 100
  )

  private class IntSequence {
    private var i   = 1
    def draw(): Int = i tap (_ => i += 1)
  }

  object `ArraySet ` {
    def `can grow`: Unit = {
      val arr = ArraySet.emptyWithHints[Int]
      arr.capacity shouldBe hints.initialCapacity

      val integers = new IntSequence
      def add(numberOfAdditions: Int, expectedCapacity: Int): Unit =
        for (i <- 0 until numberOfAdditions) {
          arr += integers.draw()
          arr.capacity shouldBe expectedCapacity
        }

      var toAdd, nextCapacity = hints.initialCapacity
      while (nextCapacity <= hints.hashTableThreshold) {
        add(toAdd, nextCapacity)
        nextCapacity += hints.capacityIncrement
        toAdd = hints.capacityIncrement
      }
      add(1, 0)
      arr.isArray shouldBe false
    }

    def `may be compacted`: Unit = {
      val integers = new IntSequence
      val toAdd    = hints.initialCapacity + 1
      val arr = ArraySet.emptyWithHints[Int] ++=
        (for (i <- 1 to toAdd) yield integers.draw())
      arr.compact()
      arr.capacity shouldBe toAdd
    }

    def `may be configured to be represented solely by a HashSet`: Unit = {
      val edges = new IntSequence
      val arr   = ArraySet.emptyWithHints[Int](ArraySet.Hints.HashOnly)
      def check(): Unit = {
        arr.isArray shouldBe false
        arr.capacity shouldBe 0
      }
      check()

      arr += edges.draw()
      check()

      arr.compact()
      check()
    }

    def `supports hints`: Unit = {
      val edges = new IntSequence
      val arr   = ArraySet.emptyWithHints[Int](ArraySet.Hints(0, 4, 8, 0))
      arr += edges.draw()
      arr.capacity shouldBe 4
    }

    def `supports hints properly when filtered`: Unit = {
      val integers = new IntSequence
      type E = Int
      val arr  = ArraySet.emptyWithHints[E]
      val size = hints.initialCapacity + 1
      for (i <- 1 to size) arr += integers.draw()

      val taken = arr take size
      taken.isInstanceOf[ArraySet[_]] shouldBe true
      taken should have size size

      def setInterface[A](set: MutableSet[A], n: Int): MutableSet[A] = set take n
      setInterface(arr, size - 1).isInstanceOf[ArraySet[_]] shouldBe true

      val filtered0 = arr filter (_ => false)
      filtered0.isInstanceOf[ArraySet[_]] shouldBe true
      taken should have size size
      filtered0.hints.initialCapacity should equal(arr.size)

      for (i <- 1 to hints.capacityIncrement) arr += integers.draw()
      val filteredEven = arr filter (_ % 2 == 0)
      filteredEven.hints.initialCapacity should equal(arr.size)
    }

    def `is sortable`: Unit = {
      val sorted = ArraySet(3, 6, 0, -3).sorted

      sorted.isInstanceOf[SortedArraySet[_]] shouldBe true
      sorted.filter(_ < 0).isInstanceOf[SortedArraySet[_]] shouldBe true
      sorted.toList shouldBe List(-3, 0, 3, 6)
      sorted.rangeFrom(1) shouldBe SortedArraySet(3, 6)
      sorted.rangeUntil(0) shouldBe SortedArraySet(-3)
      sorted.range(-10, 10) shouldBe sorted
      sorted.range(-10, 1) shouldBe SortedArraySet(-3, 0)
      sorted.range(-10, -3) shouldBe SortedArraySet.empty[Int]
      sorted.range(-10, -4) shouldBe SortedArraySet.empty[Int]
    }

    def `supports ++` : Unit = {
      val a = ArraySet.empty[Int]
      val b = ArraySet(1)
      val c = ArraySet(2)

      a.clone shouldBe empty
      a ++ b shouldBe b
      b ++ c shouldBe (b.toSet ++ c.toSet)
    }

    object `supports upsert` {
      case class Mutable(key: Int)(var i: Int = 0)

      def upsert(setSize: Int): Unit = {
        val integers = new IntSequence
        val pos      = 1
        pos shouldBe <(setSize)

        val arr = ArraySet.emptyWithHints[Mutable] ++=
          (for (i <- 1 to setSize) yield Mutable(integers.draw())())
        arr should have size setSize

        def mutable  = arr.drop(pos).head
        val newI     = mutable.i + 1
        val toUpsert = Mutable(mutable.key)(newI)
        toUpsert should equal(mutable)

        val inserted = arr.upsert(toUpsert)
        inserted shouldBe false
        mutable.i shouldBe newI

        arr.size shouldBe setSize
        arr.upsert(Mutable(integers.draw())()) should ===(true)
        arr.size shouldBe (setSize + 1)
      }

      def `when represented by an Array`: Unit =
        upsert(hints.hashTableThreshold - 3)

      def `when represented by a HashSet`: Unit =
        upsert(hints.hashTableThreshold + 3)
    }
  }
}
