package scalax.collection.concurrent

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.concurrent.ArrayTree.TIndex
import scalax.util.primitives.{Capacity, Index, Size}

import scala.collection.immutable.SortedSet
import scala.util.chaining.scalaUtilChainingOps

class TombstonesSpec extends RefSpec, Matchers, OptionValues:
  import IndexTIndex.*
  import Tombstones.*

  import scala.language.implicitConversions
  private given Conversion[Int, Index]    = (i: Int) => Index.unsafe(i)
  private given Conversion[Int, Capacity] = (i: Int) => Capacity.unsafe(i)

  object `internal search`:
    def `linear search`: Unit =
      val values = Array.tabulate(10)(i => IndexTIndex(i * 2 + 1, i * 100))

      def check(key: Index, from: Index, to: Index)(expected: SearchResult): Unit =
        require(to - from < linearUntil)
        search(key)(values, from, to) shouldBe expected

      check(key = Index(0), 0, 1)(SearchResult.insertionPoint(0))
      check(key = Index(1), 0, 2)(SearchResult.foundAt(0))
      check(key = Index(1), 1, 4)(SearchResult.insertionPoint(1))
      check(key = Index(5), 0, 4)(SearchResult.foundAt(2))
      check(key = Index(6), 0, 4)(SearchResult.insertionPoint(3))
      check(key = Index(6), 0, 2)(SearchResult.insertionPoint(3))

    def `binary search`: Unit =
      val values = Array.tabulate(30)(i => IndexTIndex(i * 2 + 1, i * 100))

      def check(key: Index, from: Index, to: Index)(expected: SearchResult): Unit =
        require(to - from >= linearUntil)
        search(key)(values, from, to) shouldBe expected

      check(key = Index(0), 0, 20)(SearchResult.insertionPoint(0))
      check(key = Index(1), 0, 20)(SearchResult.foundAt(0))
      check(key = Index(1), 1, 20)(SearchResult.insertionPoint(1))
      check(key = Index(5), 0, 20)(SearchResult.foundAt(2))
      check(key = Index(6), 0, 20)(SearchResult.insertionPoint(3))
      check(key = Index(50), 0, 20)(SearchResult.insertionPoint(21))

  object `factory `:
    def `from sorted`: Unit =
      inline def size = 5
      val values      = Array.tabulate(size)(i => IndexTIndex(i, i * 2))
      val tombstones  = Tombstones.from(values)(maxCap = 20)

      tombstones.size.value shouldBe size
      tombstones.gap.len.value should be > 0
      tombstones.iterator.toList shouldBe values.toList

    def `from unsorted`: Unit =
      inline def size = 5
      val values = Array.tabulate(size)(i => IndexTIndex(10 - i, i * 2))
      val tombstones = Tombstones.from(values)(maxCap = 20)

      tombstones.size.value shouldBe size
      tombstones.gap.len.value should be > 0
      tombstones.iterator.toList shouldBe values.sortWith((a, b) => a.index < b.index).toList

    def `with duplicates`: Unit =
      inline def size = 3
      val values = Array.fill(size)(IndexTIndex(5, 0))
      an[IllegalArgumentException] shouldBe thrownBy (Tombstones.from(values)(maxCap = 20))

  object `get `:
    def `from left`: Unit =
      val tombstones =
        val values = Array.tabulate(3)(i => IndexTIndex(i, i * 2))
        Tombstones.from(values)(maxCap = 4)
      import tombstones.get

      get(Index(0)).value shouldBe TIndex(0)
      get(Index(2)).value shouldBe TIndex(4)
      get(Index(3)) shouldBe None

    def `from left or right`: Unit =
      val tombstones = Tombstones.from(Iterator(1, 10) map (IndexTIndex(_, 0)))(maxCap = 8)
      import tombstones._
      putIfAbsent(Index(2), TIndex(1))
      gap.from shouldBe Index(2)

      get(Index(1)).value shouldBe TIndex(0)
      get(Index(2)).value shouldBe TIndex(1)
      get(Index(8)) shouldBe None
      get(Index(10)).value shouldBe TIndex(0)
      get(Index(11)) shouldBe None

  object `putIfAbsent `:
    def `when left only exists`: Unit =
      val tombstones = Tombstones.from(1 to 4 map (IndexTIndex(_, 0)))(maxCap = 8)
      import tombstones._
      gap shouldBe (Index(4), Size(4))
      putIfAbsent(4, 1) shouldBe TIndex(0)

      putIfAbsent(6, 1) shouldBe None
      gap shouldBe (Index(5), Size(3))
      keysIterator.toBuffer shouldBe ((1 to 4).toBuffer += 6)

      putIfAbsent(5, 1) shouldBe None
      gap shouldBe (Index(6), Size(2))
      keysIterator.toBuffer shouldBe (1 to 6).toBuffer

      putIfAbsent(0, 1) shouldBe None
      gap shouldBe (Index(7), Size(1))
      keysIterator.toBuffer shouldBe (0 to 6).toBuffer

      putIfAbsent(99, 1) shouldBe None
      gap.len shouldBe Size(0)
      keysIterator.toBuffer shouldBe ((0 to 6).toBuffer += 99)

    def `when both left and right exist`: Unit =
      val tombstones = Tombstones.from(List(1, 10, 11) map (IndexTIndex(_, 0)))(maxCap = 8)
      import tombstones._
      gap shouldBe (Index(3), Size(5))

      putIfAbsent(3, 1) shouldBe None
      gap shouldBe (Index(2), Size(4))
      keysIterator.toBuffer shouldBe List(1, 3, 10, 11).toBuffer

      putIfAbsent(2, 1) shouldBe None
      gap shouldBe (Index(3), Size(3))
      keysIterator.toBuffer shouldBe List(1, 2, 3, 10, 11).toBuffer

      putIfAbsent(13, 1) shouldBe None
      gap shouldBe (Index(3), Size(2))
      keysIterator.toBuffer shouldBe List(1, 2, 3, 10, 11, 13).toBuffer

    object `given exhausted capacity`:
      val exhausted = Tombstones(Index(0), TIndex.zero)(maxCap = 16) tap { t =>
        import t._
        (1 until capacity.value) foreach (i => putIfAbsent(i * 2, 0) shouldBe None)
        gap.len shouldBe Size.zero
        capacity.value should be < 16
      }
      val exhaustedKeys = SortedSet.from(exhausted.keysIterator)

      def check(add: Index): Unit =
        exhausted.putIfAbsent(add, 1) match
          case Some(increased: Tombstones) =>
            import increased._
            capacity.value should be > exhausted.capacity.value
            gap.len.value should be > 0
            increased.size.value shouldBe exhausted.size.incrTrusted.value
            keysIterator.toBuffer shouldBe (exhaustedKeys + add).toBuffer
          case _ => fail()

      def `< max contained`: Unit = check(5)
      def `> max contained`: Unit = check(77)
