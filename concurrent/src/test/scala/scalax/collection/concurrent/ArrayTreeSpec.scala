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
      info(f"$count%2d futures, tree($initialCapacity, $leafCapacity, $nodeCapacity)")
      val tree = ArrayTree[Int](initialCapacity, leafCapacity, nodeCapacity)

      val range = 0 until count
      val seq   = Future.sequence(range map (i => Future(tree append i)))
      whenReady(seq)(_.map(_.value).sum shouldBe range.sum)

      tree.size.value shouldBe count
      if tree.collisions == 0 then info("warning: expected some collisions but none detected")

    append(count = 5)(initialCapacity = 5)
    append(count = 10)(initialCapacity = 4)
    append(count = 20)(initialCapacity = 4)

  object `treeIterator, leafCapacity: 2, nodeCapacity: 2`:
    val defaultLeafCapacity = 2
    val defaultNodeCapacity = 2

    def check(size: PositiveSize)(
        initialCapacity: PositiveSize,
        leafCapacity: PositiveSize = defaultLeafCapacity,
        nodeCapacity: PositiveSize = defaultNodeCapacity
    )(expected: List[Tree[Int]]): Unit =
      val tree = ArrayTree[Int](initialCapacity, leafCapacity, nodeCapacity)
      1 to size.value foreach tree.append
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
        Node(defaultNodeCapacity, null)(Array.fill(size.value)(Multiple.fake)*)

      private def withNullParentAndFakeNode(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, null)(Array.fill(size.value)(Node.fake)*)

      private def withFakeParentAndFakeMulti(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, Node.fake)(Array.fill(size.value)(Multiple.fake)*)

      private def withFakeParentAndFakeNode(size: PositiveSize = defaultNodeCapacity): Node[Int] =
        Node(defaultNodeCapacity, Node.fake)(Array.fill(size.value)(Node.fake)*)

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
