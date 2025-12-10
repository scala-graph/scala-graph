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

class ArrayTreeSpec extends RefSpec with Matchers with ScalaFutures:
  implicit val disableDefaultArrayHandling: Prettifier = Prettifier(_.toString)

  import scala.language.implicitConversions
  private given Conversion[Int, PositiveSize] = (i: Int) => PositiveSize.unsafe(i)

  import ArrayTreeSpec.*

  def `append single threaded`: Unit =
    def check(appendCount: PositiveSize)(
        initialCapacity: PositiveSize,
        leafCapacity: PositiveSize = 2,
        nodeCapacity: PositiveSize = 2
    ): Unit =
      info(f"$appendCount%2d times to tree($initialCapacity, $leafCapacity, $nodeCapacity)")
      val tree = ArrayTree[Int](initialCapacity, leafCapacity, nodeCapacity)
      1 to appendCount.value foreach { i =>
        (tree append i).value shouldBe i - 1
        tree.size.value shouldBe i
      }
      tree.collisions shouldBe 0

    check(2)(initialCapacity = 1)
    check(4)(initialCapacity = 4)
    check(3)(initialCapacity = 1)
    check(3)(initialCapacity = 2)
    check(5)(initialCapacity = 4)
    check(7)(initialCapacity = 4)
    check(9)(initialCapacity = 4)
    check(40)(initialCapacity = 1, leafCapacity = 5, nodeCapacity = 5)

  def `append concurrently`: Unit =
    given ExecutionContext = ExecutionContext.global

    def append(count: Int)(
        initialCapacity: PositiveSize,
        leafCapacity: PositiveSize = 2,
        nodeCapacity: PositiveSize = 2
    ): Unit =
      given tree: ArrayTree[Int] = ArrayTree[Int](initialCapacity, leafCapacity, nodeCapacity)
      val range                  = 0 until count
      val seq                    = Future.sequence(range map (i => Future(tree append i)))
      withClue(f"$count%2d futures, tree($initialCapacity, $leafCapacity, $nodeCapacity)$lineSeparator")(
        whenReady(seq) { indexes =>
          indexes should coverRange(range)
          tree.size.value shouldBe count
          if tree.collisions == 0 then info("warning: expected some collisions but none detected")
        }
      )

    append(count = 5)(initialCapacity = 5)
    append(count = 10)(initialCapacity = 4)
    append(count = 20)(initialCapacity = 4)
    append(count = 25)(initialCapacity = 3)

  object `treeIterator, leafCapacity: 2, nodeCapacity: 2`:
    val defaultLeafCapacity = 2
    val defaultNodeCapacity = 2

    def tree(size: PositiveSize)(
        initialCapacity: PositiveSize,
        leafCapacity: PositiveSize = defaultLeafCapacity,
        nodeCapacity: PositiveSize = defaultNodeCapacity
    ): ArrayTree[Int] =
      ArrayTree[Int](initialCapacity, leafCapacity, nodeCapacity) tap (t => 1 to size.value foreach t.append)

    extension (multi: MultiLeaf.type)
      private def fake: MultiLeaf[Int] =
        MultiLeaf.empty[Int](defaultLeafCapacity, null)

      private def withNullParent(elems: Int*): MultiLeaf[Int] =
        MultiLeaf(defaultLeafCapacity, null)(elems*)

      private def withFakeParent(elems: Int*): MultiLeaf[Int] =
        MultiLeaf(defaultLeafCapacity, LeafParentNode.fake)(elems*)

    extension (node: UpperNode.type)
      private def fake: UpperNode[Int] =
        UpperNode.empty[Int](defaultNodeCapacity, null)

      private def fullFakeWithNullParent: UpperNode[Int] =
        UpperNode(defaultNodeCapacity, null)(Array.fill(defaultNodeCapacity)(UpperNode.fake)*)

      private def fullFakeWithFakeParent: UpperNode[Int] =
        UpperNode(defaultNodeCapacity, UpperNode.fake)(Array.fill(defaultNodeCapacity)(UpperNode.fake)*)

      private def fakeWithFakeParent(elemCount: PositiveSize): UpperNode[Int] =
        UpperNode(defaultNodeCapacity, UpperNode.fake)(Array.fill(elemCount.value)(UpperNode.fake)*)

    extension (node: LeafParentNode.type)
      private def fake: LeafParentNode[Int] =
        LeafParentNode.empty[Int](defaultNodeCapacity, null)

      private def fullFakeWithNullParent: LeafParentNode[Int] =
        LeafParentNode(defaultNodeCapacity, null)(Array.fill(defaultNodeCapacity)(MultiLeaf.fake)*)

      private def fullFakeWithFakeParent: LeafParentNode[Int] =
        LeafParentNode(defaultNodeCapacity, UpperNode.fake)(Array.fill(defaultNodeCapacity)(MultiLeaf.fake)*)

      private def fakeWithFakeParent(elemCount: PositiveSize): LeafParentNode[Int] =
        LeafParentNode(defaultNodeCapacity, UpperNode.fake)(Array.fill(elemCount.value)(MultiLeaf.fake)*)

    def `size:  1, initialCapacity: 1`: Unit =
      tree(1)(initialCapacity = 1) should equalTree(SingleLeaf(1))

    def `size:  2, initialCapacity: 1`: Unit =
      tree(2)(initialCapacity = 1) should equalTree(MultiLeaf.withNullParent(1, 2))

    def `size:  3, initialCapacity: 1`: Unit =
      tree(3)(initialCapacity = 1) should equalTree(
        LeafParentNode.fullFakeWithNullParent,
        MultiLeaf.withFakeParent(1, 2),
        MultiLeaf.withFakeParent(3)
      )

    def `size:  3, initialCapacity: 2`: Unit =
      tree(3)(initialCapacity = 1) should equalTree(
        LeafParentNode.fullFakeWithNullParent,
        MultiLeaf.withFakeParent(1, 2),
        MultiLeaf.withFakeParent(3)
      )

    def `size:  5, initialCapacity: 4`: Unit =
      tree(5)(initialCapacity = 4) should equalTree(
        LeafParentNode.fullFakeWithNullParent,
        MultiLeaf(4, UpperNode.fake)(1, 2, 3, 4),
        MultiLeaf.withFakeParent(5)
      )

    def `size:  7, initialCapacity: 4`: Unit =
      tree(7)(initialCapacity = 4) should equalTree(
        UpperNode.fullFakeWithNullParent,
        LeafParentNode.fullFakeWithFakeParent,
        MultiLeaf(4, UpperNode.fake)(1, 2, 3, 4),
        MultiLeaf.withFakeParent(5, 6),
        LeafParentNode.fakeWithFakeParent(elemCount = 1),
        MultiLeaf.withFakeParent(7)
      )

    def `size: 12, initialCapacity: 4`: Unit =
      tree(12)(initialCapacity = 4) should equalTree(
        UpperNode.fullFakeWithNullParent,
        UpperNode.fullFakeWithFakeParent,
        LeafParentNode.fullFakeWithFakeParent,
        MultiLeaf(4, UpperNode.fake)(1, 2, 3, 4),
        MultiLeaf.withFakeParent(5, 6),
        LeafParentNode.fullFakeWithFakeParent,
        MultiLeaf.withFakeParent(7, 8),
        MultiLeaf.withFakeParent(9, 10),
        UpperNode.fakeWithFakeParent(elemCount = 1),
        LeafParentNode.fakeWithFakeParent(elemCount = 1),
        MultiLeaf.withFakeParent(11, 12)
      )

  object `config `:
    import ArrayTree.Config
    import ArrayTree.Config.LevelCap.*

    def `initial length`: Unit =
      Config(
        initialCapacity = PositiveSize(10),
        leafCapacity = PositiveSize(4),
        nodeCapacity = PositiveSize(3)
      ).levelCaps should have length 8

    def `full levelSizes`: Unit =
      Config(
        initialCapacity = PositiveSize(1000),
        leafCapacity = PositiveSize(200),
        nodeCapacity = PositiveSize(4)
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
      val config = Config(PositiveSize(1000), PositiveSize(100), PositiveSize(10))
      config.levelCaps.last match
        case Full(_, _, total) =>
          config.extendLevelCaps shouldBe true
          config.levelCaps.last shouldBe a[Partial]
        case p: Partial =>
          fail()

    def `forEachLevelCap `: Unit =
      var actual = List.empty[LevelCap]
      Config(
        initialCapacity = PositiveSize(1000),
        leafCapacity = PositiveSize(200),
        nodeCapacity = PositiveSize(4)
      ).forEachLevelCap(Positive(4))(cap => actual = cap +: actual)
      actual shouldBe List(
        Full(1_000, 200, 1_600),
        Full(1_600, 800, 4_000),
        Full(4_000, 3_200, 13_600),
        Full(13_600, 12_800, 52_000)
      )

    object `propagateDown `:
      val initialCap = PositiveSize(100)
      val config     = Config(
        initialCapacity = initialCap,
        leafCapacity = PositiveSize(10),
        nodeCapacity = PositiveSize(4)
      )
      val (node, leaf) = ('n', "Leaf")

      def `index zero`: Unit =
        config.propagateDown(root = node, startHeight = Positive(1), index = IntIndex.zero)(
          (a: Char, slot: IntIndex) =>
            slot shouldBe IntIndex.zero
            node
          ,
          (a: Char, slot: IntIndex) =>
            slot shouldBe IntIndex.zero
            leaf
        ) shouldBe leaf

      def `index below initial capacity`: Unit =
        val index = IntIndex(99)
        config.propagateDown(root = node, startHeight = Positive(4), index)(
          (a: Char, slot: IntIndex) =>
            slot shouldBe IntIndex.zero
            node
          ,
          (a: Char, slot: IntIndex) =>
            slot shouldBe index
            leaf
        ) shouldBe leaf

      def `index in first subsequent leaf `: Unit =
        val index = IntIndex(105)
        config.propagateDown(root = node, startHeight = Positive(4), index)(
          (a: Char, slot: IntIndex) =>
            slot shouldBe IntIndex.zero
            node
          ,
          (a: Char, slot: IntIndex) =>
            slot shouldBe index.mapTrusted(_ - initialCap.value)
            leaf
        ) shouldBe leaf

      def `index in the mid of the tree `: Unit =
        val index         = IntIndex(2001)
        val expectedSlots = Array(2, 3, 3)
        var count: Int    = 0
        config.propagateDown(root = node, startHeight = Positive(4), index)(
          (a: Char, slot: IntIndex) =>
            withClue(s"Count: $count")(slot.value shouldBe expectedSlots(count))
            count += 1
            node
          ,
          (a: Char, slot: IntIndex) =>
            withClue(s"Height 1")(slot shouldBe IntIndex(1))
            leaf
        ) shouldBe leaf

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

  def coverRange(expectedIndexes: Range)(using tree: ArrayTree[Int]): Matcher[IndexedSeq[Index]] =
    Matcher { (indexes: IndexedSeq[Index]) =>
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
