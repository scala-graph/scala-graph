package scalax.collection.concurrent

import scala.concurrent.{ExecutionContext, Future}

import org.scalactic.Prettifier
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.util.primitives.*

class ArrayTreeSpec extends RefSpec with Matchers with ScalaFutures:
  implicit val disableDefaultArrayHandling: Prettifier = Prettifier(_.toString)

  import scala.language.implicitConversions
  private given Conversion[Int, PositiveSize] = (i: Int) => PositiveSize.unsafe(i)

  import ArrayTree.*

  object `append single threaded`:
    def `returns expected LongIndex, and tree size`: Unit =
      def check(appendCount: PositiveSize)(
          initialCapacity: PositiveSize,
          leafCapacity: PositiveSize = PositiveSize(2),
          nodeCapacity: PositiveSize = PositiveSize(2)
      ): Unit =
        info(f"$appendCount%2d times to tree($initialCapacity, $leafCapacity, $nodeCapacity)")
        val tree = ArrayTree[Int](initialCapacity, leafCapacity, nodeCapacity)
        1 to appendCount.toInt foreach { i =>
          (tree append i).toInt shouldBe i - 1
          tree.size.toInt shouldBe i
        }
        tree.collisions shouldBe 0

      check(2)(initialCapacity = 1)
      check(4)(initialCapacity = 4)
      check(3)(initialCapacity = 1)
      check(3)(initialCapacity = 2)
      check(5)(initialCapacity = 4)
      check(7)(initialCapacity = 4)
      check(9)(initialCapacity = 4)
      check(40)(initialCapacity = 1, leafCapacity = PositiveSize(5), nodeCapacity = 5)

    def `has expected tree structure`: Unit =
      val leafCapacity = 4
      val nodeCapacity = 2
      val sample       = ArrayTree[Int](1, leafCapacity, nodeCapacity)
      sample.tree shouldBe null

      sample append 1
      sample.tree shouldBe Single(1)

      sample append 2
      sample.tree shouldBe Multiple[Int](leafCapacity, null)(1, 2)
      val multi1 = sample.tree.asInstanceOf[Multiple[Int]]

      3 to 5 foreach sample.append
      def fakeMulti = Multiple.empty[Int](leafCapacity, null)
      sample.tree shouldBe Node[Int](nodeCapacity, null)(multi1, fakeMulti)
      val node1 = sample.tree.asInstanceOf[Node[Int]]
      node1 match
        case Node(elems, parent) =>
          elems(0) should be theSameInstanceAs multi1
          elems(0) shouldBe Multiple[Int](leafCapacity, multi1.parent)(1, 2, 3, 4)
          multi1.parent should be theSameInstanceAs sample.tree

          val multi2 = elems(1)
          multi2 shouldBe Multiple[Int](leafCapacity, multi1.parent)(5)

  // TODO Retry
  def `append concurrently`: Unit =
    given ExecutionContext = ExecutionContext.global

    def check(futureCount: Int)(
        initial: PositiveSize,
        leavesSize: PositiveSize = PositiveSize(2),
        nodeSize: PositiveSize = PositiveSize(2)
    ): Unit =
      info(f"$futureCount%2d futures, tree($initial, $leavesSize, $nodeSize)")
      val tree = ArrayTree[Int](initial, leavesSize, nodeSize)

      val range = 0 until futureCount
      val seq   = Future.sequence(range map (i => Future(tree append i)))
      whenReady(seq)(_.map(_.toInt).sum shouldBe range.sum)

      tree.size.toInt shouldBe futureCount
      tree.collisions should be > 0L

    check(5)(initial = PositiveSize(5))
    check(10)(initial = PositiveSize(5))

  object `treeIterator, leafCapacity: 2, nodeCapacity: 2`:
    val defaultLeafCapacity = PositiveSize(2)
    val defaultNodeCapacity = PositiveSize(2)

    def check(size: PositiveSize)(
        initialCapacity: PositiveSize,
        leafCapacity: PositiveSize = defaultLeafCapacity,
        nodeCapacity: PositiveSize = defaultNodeCapacity
    )(expected: List[Tree[Int]]): Unit =
      val tree = ArrayTree[Int](initialCapacity, leafCapacity, nodeCapacity)
      1 to size.toInt foreach tree.append
      tree.treeIterator.toList shouldBe expected

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
        Node(defaultNodeCapacity, null)(Array.fill(size.toInt)(Multiple.fake)*)

      private def withNullParentAndFakeNode(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, null)(Array.fill(size.toInt)(Node.fake)*)

      private def withFakeParentAndFakeMulti(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, Node.fake)(Array.fill(size.toInt)(Multiple.fake)*)

      private def withFakeParentAndFakeNode(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, Node.fake)(Array.fill(size.toInt)(Node.fake)*)

    def `size:  1, initialCapacity: 1`: Unit =
      check(1)(initialCapacity = 1)(Single(1) :: Nil)

    def `size:  2, initialCapacity: 1`: Unit =
      check(2)(initialCapacity = 1)(Multiple.withNullParent(1, 2) :: Nil)

    def `size:  3, initialCapacity: 1`: Unit =
      check(3)(initialCapacity = 1)(
        Node.withNullParentAndFakeMulti() ::
          Multiple.withFakeParent(1, 2) ::
          Multiple.withFakeParent(3) ::
          Nil
      )

    def `size:  3, initialCapacity: 2`: Unit =
      check(3)(initialCapacity = 1)(
        Node.withNullParentAndFakeMulti() ::
          Multiple.withFakeParent(1, 2) ::
          Multiple.withFakeParent(3) ::
          Nil
      )

    def `size:  5, initialCapacity: 4`: Unit =
      check(5)(initialCapacity = 4)(
        Node.withNullParentAndFakeMulti() ::
          Multiple(4, Node.fake)(1, 2, 3, 4) ::
          Multiple.withFakeParent(5) ::
          Nil
      )

    def `size:  7, initialCapacity: 4`: Unit =
      check(7)(initialCapacity = 4)(
        Node.withNullParentAndFakeNode() ::
          Node.withFakeParentAndFakeMulti() ::
          Multiple(4, Node.fake)(1, 2, 3, 4) ::
          Multiple.withFakeParent(5, 6) ::
          Node.withFakeParentAndFakeMulti(1) ::
          Multiple.withFakeParent(7) ::
          Nil
      )

    def `size: 12, initialCapacity: 4`: Unit =
      check(12)(initialCapacity = 4)(
        Node.withNullParentAndFakeNode() ::
          Node.withFakeParentAndFakeNode() ::
          Node.withFakeParentAndFakeMulti() ::
          Multiple(4, Node.fake)(1, 2, 3, 4) ::
          Multiple.withFakeParent(5, 6) ::
          Node.withFakeParentAndFakeMulti() ::
          Multiple.withFakeParent(7, 8) ::
          Multiple.withFakeParent(9, 10) ::
          Node.withFakeParentAndFakeNode(1) ::
          Node.withFakeParentAndFakeMulti(1) ::
          Multiple.withFakeParent(11, 12) ::
          Nil
      )
