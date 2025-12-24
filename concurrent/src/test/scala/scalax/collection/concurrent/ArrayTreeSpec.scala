package scalax.collection.concurrent

import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps
import org.scalactic.Prettifier
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.{LazyArg, MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.concurrent.ArrayTree.Config.LevelCap
import scalax.util.primitives.*
import ArrayTree.*

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

class ArrayTreeSpec extends RefSpec with Matchers with ScalaFutures:
  implicit val disableDefaultArrayHandling: Prettifier = Prettifier(_.toString)

  import scala.language.implicitConversions
  private given Conversion[Int, Size]         = (i: Int) => Size.unsafe(i)
  private given Conversion[Int, PositiveSize] = (i: Int) => PositiveSize.unsafe(i)

  import ArrayTreeSpec.*

  def `append single threaded`: Unit =
    def check(appendCount: PositiveSize)(
        initialCap: Capacity,
        leafCap: Capacity = 2,
        nodeCap: Capacity = 2
    ): Unit =
      info(f"$appendCount%2d times to tree($initialCap, $leafCap, $nodeCap)")
      val tree = ArrayTree[Int](Config(initialCap, leafCap, nodeCap))
      1 to appendCount.value foreach { i =>
        (tree append i).value shouldBe i - 1
        tree.size.value shouldBe i
      }
      tree.collisions shouldBe 0

    check(2)(initialCap = 1)
    check(4)(initialCap = 4)
    check(3)(initialCap = 1)
    check(3)(initialCap = 2)
    check(5)(initialCap = 4)
    check(7)(initialCap = 4)
    check(9)(initialCap = 4)
    check(40)(initialCap = 1, leafCap = 5, nodeCap = 5)

  def `append concurrently`: Unit =
    given ExecutionContext = ExecutionContext.global

    def append(count: Int)(
        initialCap: Capacity,
        leafCap: Capacity = 2,
        nodeCap: Capacity = 2
    ): Unit =
      given tree: ArrayTree[Int] = ArrayTree[Int](Config(initialCap, leafCap, nodeCap))
      val range                  = 0 until count
      val seq                    = Future.sequence(range map (i => Future(tree append i)))
      withClue(f"$count%2d futures, tree($initialCap, $leafCap, $nodeCap)$lineSeparator")(
        whenReady(seq) { indexes =>
          indexes should coverRange(range)
          tree.size.value shouldBe count
          if tree.collisions == 0 then info("warning: expected some collisions but none detected")
        }
      )

    append(count = 5)(initialCap = 5)
    append(count = 10)(initialCap = 4)
    append(count = 20)(initialCap = 4)
    append(count = 25)(initialCap = 3)

  object `treeIterator, leafCapacity: 2, nodeCapacity: 2`:
    private val defaultLeafCapacity = 2
    private val defaultNodeCapacity = 2

    private def tree(size: PositiveSize)(
        initialCap: Capacity,
        leafCap: Capacity = defaultLeafCapacity,
        nodeCap: Capacity = defaultNodeCapacity
    ): ArrayTree[Int] =
      ArrayTree[Int](Config(initialCap, leafCap, nodeCap)) tap (t => 1 to size.value foreach t.append)

    extension (multi: MultiLeaf.type)
      private def fake: MultiLeaf[Int] =
        MultiLeaf.empty[Int](defaultLeafCapacity, null)

      private def withNullLeftNeighbor(elems: Int*): MultiLeaf[Int] =
        MultiLeaf(defaultLeafCapacity, null)(elems*)

      private def withFakeLeftNeighbor(elems: Int*): MultiLeaf[Int] =
        MultiLeaf(defaultLeafCapacity, fake)(elems*)

    extension (node: UpperNode.type)
      private def fake: UpperNode[Int] =
        UpperNode.empty[Int](defaultNodeCapacity)

      private def denseFake: UpperNode[Int] =
        UpperNode(defaultNodeCapacity)(Array.fill(defaultNodeCapacity)(UpperNode.fake)*)

      private def sparseFake(elemCount: PositiveSize): UpperNode[Int] =
        UpperNode(defaultNodeCapacity)(Array.fill(elemCount.value)(UpperNode.fake)*)

    extension (node: LeafParentNode.type)
      private def denseFake: LeafParentNode[Int] =
        LeafParentNode(defaultNodeCapacity)(Array.fill(defaultNodeCapacity)(MultiLeaf.fake)*)

      private def sparseFake(elemCount: PositiveSize): LeafParentNode[Int] =
        LeafParentNode(defaultNodeCapacity)(Array.fill(elemCount.value)(MultiLeaf.fake)*)

    def `size:  1, initialCapacity: 1`: Unit =
      tree(1)(initialCap = 1) should equalTree(SingleLeaf(1))

    def `size:  2, initialCapacity: 1`: Unit =
      tree(2)(initialCap = 1) should equalTree(MultiLeaf.withNullLeftNeighbor(1, 2))

    def `size:  3, initialCapacity: 1`: Unit =
      tree(3)(initialCap = 1) should equalTree(
        LeafParentNode.denseFake,
        MultiLeaf.withNullLeftNeighbor(1, 2),
        MultiLeaf.withFakeLeftNeighbor(3)
      )

    def `size:  3, initialCapacity: 2`: Unit =
      tree(3)(initialCap = 1) should equalTree(
        LeafParentNode.denseFake,
        MultiLeaf.withNullLeftNeighbor(1, 2),
        MultiLeaf.withFakeLeftNeighbor(3)
      )

    def `size:  5, initialCapacity: 4`: Unit =
      tree(5)(initialCap = 4) should equalTree(
        LeafParentNode.denseFake,
        MultiLeaf(4, null)(1, 2, 3, 4),
        MultiLeaf.withFakeLeftNeighbor(5)
      )

    def `size:  7, initialCapacity: 4`: Unit =
      tree(7)(initialCap = 4) should equalTree(
        UpperNode.denseFake,
        LeafParentNode.denseFake,
        MultiLeaf(4, null)(1, 2, 3, 4),
        MultiLeaf.withFakeLeftNeighbor(5, 6),
        LeafParentNode.sparseFake(elemCount = 1),
        MultiLeaf.withFakeLeftNeighbor(7)
      )

    def `size: 12, initialCapacity: 4`: Unit =
      tree(12)(initialCap = 4) should equalTree(
        UpperNode.denseFake,
        UpperNode.denseFake,
        LeafParentNode.denseFake,
        MultiLeaf(4, null)(1, 2, 3, 4),
        MultiLeaf.withFakeLeftNeighbor(5, 6),
        LeafParentNode.denseFake,
        MultiLeaf.withFakeLeftNeighbor(7, 8),
        MultiLeaf.withFakeLeftNeighbor(9, 10),
        UpperNode.sparseFake(elemCount = 1),
        LeafParentNode.sparseFake(elemCount = 1),
        MultiLeaf.withFakeLeftNeighbor(11, 12)
      )

  object `apply index`:
    val initialOneConfig = Config(initialCap = 1, leafCap = 10, nodeCap = 4)
    val smallConfig      = Config(initialCap = 20, leafCap = 10, nodeCap = 4)

    private def smallConfigLevelCaps(i: Int): LevelCap.Full =
      smallConfig.levelCaps(i) match
        case full: LevelCap.Full => full
        case _                   => fail()

    private def populateAndCheck(size: PositiveSize): Unit =
      val tree = ArrayTree[Int](smallConfig)
      size.indexes foreach tree.append

      Range(start = 0, end = size.value - 1, step = 3) foreach { i =>
        tree(TIndex.trust(i)) shouldBe i
      }
      an[IndexOutOfBoundsException] shouldBe thrownBy(tree(size.asNonNegative))

    def `empty tree`: Unit =
      val tree = ArrayTree[Int](initialOneConfig)
      an[IndexOutOfBoundsException] shouldBe thrownBy(tree(TIndex(0)))

    def `root SingleLeaf`: Unit =
      val tree = ArrayTree[Int](initialOneConfig)
      tree append 7
      tree(TIndex.zero) shouldBe 7
      an[IndexOutOfBoundsException] shouldBe thrownBy(tree(TIndex(1)))

    def `root MultiLeaf`: Unit =
      populateAndCheck(smallConfig.initialCap)

    def `root LeafParentNode`: Unit =
      val caps0 = smallConfigLevelCaps(0)
      populateAndCheck(caps0.first + caps0.subsequent)

    def `root UpperNode`: Unit =
      val caps1 = smallConfigLevelCaps(1)
      populateAndCheck(caps1.first + caps1.subsequent * 3 + 2)

  object `config `:
    import ArrayTree.Config
    import ArrayTree.Config.LevelCap.*

    def `initial length`: Unit =
      Config(
        initialCap = 10,
        leafCap = 4,
        nodeCap = 3
      ).levelCaps should have length 8

    def `full levelSizes`: Unit =
      Config(
        initialCap = 1000,
        leafCap = 200,
        nodeCap = 4
      ).levelCaps should contain theSameElementsInOrderAs List(
        Full(1_000, 200, 1_600),
        Full(1_600, 800, 4_000),
        Full(4_000, 3_200, 13_600),
        Full(13_600, 12_800, 52_000),
        Full(52_000, 51_200, 205_600),
        Full(205_600, 204_800, 820_000),
        Full(820_000, 819_200, 3_277_600),
        Full(3_277_600, 3_276_800, 13_108_000)
      )

    def `partial levelSizes`: Unit =
      val config = Config(Capacity(1000), Capacity(100), Capacity(10))
      config.levelCaps.last match
        case Full(_, _, total) =>
          config.extendLevelCaps shouldBe true
          config.levelCaps.last shouldBe a[Partial]
        case p: Partial =>
          fail()

    object `propagate index`:
      val initialCap = Capacity(100)
      val config     = Config(
        initialCap = initialCap,
        leafCap = Capacity(10),
        nodeCap = Capacity(4)
      )

      def `illustrate levelCaps of config`: Unit =
        /* height left side     non-left side
         * ----------------------------------
         *      5     2_650
         *      4       730               640
         *      3       730               640
         *      2       250               120
         *      1       130                40
         *      0       100 10 10 10       10 10 10 10
         */
        config.levelCaps.take(4) should contain theSameElementsInOrderAs List(
          Full(100, 10, 130),
          Full(130, 40, 250),
          Full(250, 160, 730),
          Full(730, 640, 2650)
        )

      def `index zero`: Unit =
        propagatedIndexes(startHeight = 1, index = TIndex.zero) shouldBe ArraySeq(0, 0)

      def `index below initial capacity`: Unit =
        val index = TIndex(99)
        propagatedIndexes(startHeight = 4, index) shouldBe ArraySeq(0, 0, 0, 0, index)

      def `index in first subsequent leaf`: Unit =
        val index = TIndex(105)
        propagatedIndexes(startHeight = 4, index) shouldBe
          ArraySeq(0, 0, 0, 1, index.value - initialCap.value)

      def `index in the middle of the tree`: Unit =
        propagatedIndexes(startHeight = 4, TIndex(2001)) shouldBe ArraySeq(2, 3, 3, 3, 1)

      def `index in the last leaf`: Unit =
        propagatedIndexes(startHeight = 4, TIndex(2648)) shouldBe ArraySeq(3, 3, 3, 3, 8)

      private def propagatedIndexes[A](startHeight: Positive, index: TIndex, leftSide: Boolean = true): ArraySeq[Int] =
        import config.*
        withLevelCaps(startHeight) { height =>
          val buf = new Array[Index](height.incr.value)

          @tailrec def loop(capIndex: Index, leftSide: Boolean, i: TIndex, arrayIndex: Int): Index =
            val (slot, subIndex, _) = levelCaps(capIndex.value).locate(i, leftSide)
            buf(arrayIndex) = slot
            if capIndex.value > 0 then loop(capIndex.decr, leftSide && slot === 0, subIndex, arrayIndex + 1)
            else subIndex

          buf(height.value) = loop(height.asNonNegative.decr, leftSide, index, 0)
          ArraySeq.ofInt(buf.asInstanceOf[Array[Int]])
        }

  object `reverse iterator`:
    val config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size): Unit =
      val tree     = ArrayTree[Int](config) tap (t => 1 to size.value foreach t.append)
      val expected = Array.tabulate(size.value)(size.value - _)
      tree.reverseIterator.toBuffer should contain theSameElementsInOrderAs expected

    def `size:  0`: Unit = check(0)
    def `size:  1`: Unit = check(1)
    def `size:  2`: Unit = check(2)
    def `size:  5`: Unit = check(5)
    def `size: 10`: Unit = check(10)
    def `size: 21`: Unit = check(21)

  object `reverse iterator from`:
    val config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size, from: TIndex): Unit =
      val tree         = ArrayTree[Int](config) tap (t => 1 to size.value foreach t.append)
      val expectedSize = from.incr.value
      val expected     = Array.tabulate(expectedSize)(expectedSize - _)
      tree.reverseIterator(from).toBuffer should contain theSameElementsInOrderAs expected

    def `size:  1, from  0`: Unit = check(1, 0)
    def `size:  2, from  1`: Unit = check(2, 1)
    def `size:  5, from  3`: Unit = check(5, 3)
    def `size: 10, from  7`: Unit = check(10, 7)
    def `size: 21, from 11`: Unit = check(21, 11)
    def `size: 21, from 21`: Unit = an[IndexOutOfBoundsException] shouldBe thrownBy(check(21, 21))

  object `reverse iterator with index`:
    val config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size): Unit =
      val tree     = ArrayTree[Int](config) tap (t => 1 to size.value foreach t.append)
      val expected = Array.tabulate(size.value)(n => (size.value - n) -> (size.value - n - 1))
      tree.reverseIteratorWithIndex.toBuffer should contain theSameElementsInOrderAs expected

    def `size:  0`: Unit = check(0)
    def `size:  1`: Unit = check(1)
    def `size:  2`: Unit = check(2)
    def `size:  5`: Unit = check(5)
    def `size: 10`: Unit = check(10)
    def `size: 21`: Unit = check(21)

  object `reverse iterator from with index`:
    val config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size, from: TIndex): Unit =
      val tree         = ArrayTree[Int](config) tap (t => 1 to size.value foreach t.append)
      val expectedSize = from.incr.value
      val expected     = Array.tabulate(expectedSize)(n => (expectedSize - n) -> (expectedSize - n - 1))
      tree.reverseIteratorWithIndex(from).toBuffer should contain theSameElementsInOrderAs expected

    def `size:  1, from  0`: Unit = check(1, 0)
    def `size:  2, from  1`: Unit = check(2, 1)
    def `size:  5, from  3`: Unit = check(5, 3)
    def `size: 10, from  7`: Unit = check(10, 7)
    def `size: 21, from 11`: Unit = check(21, 11)
    def `size: 21, from 21`: Unit = an[IndexOutOfBoundsException] shouldBe thrownBy(check(21, 21))

object ArrayTreeSpec:
  private val lineSeparator = System.lineSeparator
  private val sep           = s"$lineSeparator  "

  def equalTree(expected: Tree[Int]*): Matcher[ArrayTree[Int]] =
    Matcher { (tree: ArrayTree[Int]) =>
      def msg(key: String): String =
        s"""The tree
           |${tree.prettifyTree(includeNodes = true)}
           |$key the elements
           |  ${expected mkString sep}.
         """.stripMargin

      MatchResult(
        tree.treeIterator.toList == expected,
        "{0}",
        "{1}",
        Vector(
          LazyArg("does not have")(key => msg(key.toString)),
          LazyArg("has")(key => key.toString)
        )
      )
    }

  def coverRange(expectedIndexes: Range)(using tree: ArrayTree[Int]): Matcher[IndexedSeq[TIndex]] =
    Matcher { (indexes: IndexedSeq[TIndex]) =>
      def msg(key: String): String =
        s"""The actual indexes returned by `append`
           |  ${indexes.map(_.value).sorted mkString ", "}
           |$key to $expectedIndexes in the tree
           |${tree.prettifyTree(includeNodes = true)}
         """.stripMargin

      MatchResult(
        indexes.map(_.value).sum == expectedIndexes.sum,
        "{0}",
        "{1}",
        Vector(
          LazyArg("do not correspond")(key => msg(key.toString)),
          LazyArg("correspond")(key => key.toString)
        )
      )
    }
