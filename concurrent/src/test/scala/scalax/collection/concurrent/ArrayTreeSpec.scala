package scalax.collection.concurrent

import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps

import org.scalactic.Prettifier
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

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

    extension (multi: Multiple.type)
      private def fake: Multiple[Int] =
        Multiple.empty[Int](defaultLeafCapacity, null)

      private def withNullParent(elems: Int*): Multiple[Int] =
        Multiple(defaultLeafCapacity, null)(elems*)

      private def withFakeParent(elems: Int*): Multiple[Int] =
        Multiple(defaultLeafCapacity, Node.fake)(elems*)

    extension (node: Node.type)
      private def fake: Node[Int] =
        Node.empty[Int](defaultNodeCapacity, null)

      private def withNullParentAndFakeMulti(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, null)(Array.fill(size.value)(Multiple.fake)*)

      private def withNullParentAndFakeNode(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, null)(Array.fill(size.value)(Node.fake)*)

      private def withFakeParentAndFakeMulti(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, Node.fake)(Array.fill(size.value)(Multiple.fake)*)

      private def withFakeParentAndFakeNode(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, Node.fake)(Array.fill(size.value)(Node.fake)*)

    def `size:  1, initialCapacity: 1`: Unit =
      tree(1)(initialCapacity = 1) should equalTree(Single(1))

    def `size:  2, initialCapacity: 1`: Unit =
      tree(2)(initialCapacity = 1) should equalTree(Multiple.withNullParent(1, 2))

    def `size:  3, initialCapacity: 1`: Unit =
      tree(3)(initialCapacity = 1) should equalTree(
        Node.withNullParentAndFakeMulti(),
        Multiple.withFakeParent(1, 2),
        Multiple.withFakeParent(3)
      )

    def `size:  3, initialCapacity: 2`: Unit =
      tree(3)(initialCapacity = 1) should equalTree(
        Node.withNullParentAndFakeMulti(),
        Multiple.withFakeParent(1, 2),
        Multiple.withFakeParent(3)
      )

    def `size:  5, initialCapacity: 4`: Unit =
      tree(5)(initialCapacity = 4) should equalTree(
        Node.withNullParentAndFakeMulti(),
        Multiple(4, Node.fake)(1, 2, 3, 4),
        Multiple.withFakeParent(5)
      )

    def `size:  7, initialCapacity: 4`: Unit =
      tree(7)(initialCapacity = 4) should equalTree(
        Node.withNullParentAndFakeNode(),
        Node.withFakeParentAndFakeMulti(),
        Multiple(4, Node.fake)(1, 2, 3, 4),
        Multiple.withFakeParent(5, 6),
        Node.withFakeParentAndFakeMulti(1),
        Multiple.withFakeParent(7)
      )

    def `size: 12, initialCapacity: 4`: Unit =
      tree(12)(initialCapacity = 4) should equalTree(
        Node.withNullParentAndFakeNode(),
        Node.withFakeParentAndFakeNode(),
        Node.withFakeParentAndFakeMulti(),
        Multiple(4, Node.fake)(1, 2, 3, 4),
        Multiple.withFakeParent(5, 6),
        Node.withFakeParentAndFakeMulti(),
        Multiple.withFakeParent(7, 8),
        Multiple.withFakeParent(9, 10),
        Node.withFakeParentAndFakeNode(1),
        Node.withFakeParentAndFakeMulti(1),
        Multiple.withFakeParent(11, 12)
      )

object ArrayTreeSpec:
  val lineSeparator = System.lineSeparator
  private val sep   = s"$lineSeparator  "

  def equalTree(expected: Tree[Int]*): Matcher[ArrayTree[Int]] =
    Matcher { (tree: ArrayTree[Int]) =>
      def msg(key: String): String =
        s"""The tree
           |${tree.prettifyTree(includeNodes = true)}
           |$key the elements
           |  ${expected mkString sep}.
         """.stripMargin

      MatchResult(tree.treeIterator.toList == expected, msg("does not have"), msg("has"))
    }

  def coverRange(expectedIndexes: Range)(using tree: ArrayTree[Int]): Matcher[IndexedSeq[Index]] =
    Matcher { (indexes: IndexedSeq[Index]) =>
      def msg(key: String): String =
        s"""The actual indexes returned by `append`
           |  ${indexes.map(_.value).sorted mkString ", "}
           |$key to $expectedIndexes in the tree
           |${tree.prettifyTree(includeNodes = true)}
         """.stripMargin

      MatchResult(indexes.map(_.value).sum == expectedIndexes.sum, msg("do not correspond"), msg("correspond"))
    }
