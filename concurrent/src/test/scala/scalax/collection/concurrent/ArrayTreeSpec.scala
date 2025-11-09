package scalax.collection.concurrent

import scala.concurrent.{ExecutionContext, Future}

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.util.primitives.*

class ArrayTreeSpec extends RefSpec with Matchers with ScalaFutures:
  import scala.language.implicitConversions
  private given Conversion[Int, PositiveSize] = (i: Int) => PositiveSize.unsafe(i)

  import ArrayTree.*

  object `append single threaded`:
    def `returns expected LongIndex, and tree size`: Unit =
      def check(appendCount: PositiveSize)(
          initial: PositiveSize,
          leavesSize: PositiveSize = PositiveSize(2),
          nodeSize: PositiveSize = PositiveSize(2)
      ): Unit =
        info(f"$appendCount%2d times to tree($initial, $leavesSize, $nodeSize)")
        val tree = ArrayTree[Int](initial, leavesSize, nodeSize)
        1 to appendCount.toInt foreach { i =>
          (tree append i).toInt shouldBe i - 1
          tree.size.toInt shouldBe i
        }
        tree.collisions shouldBe 0

      check(2)(initial = 1)
      check(4)(initial = 4)
      check(3)(initial = 1)
      check(3)(initial = 2)
      check(5)(initial = 4)
      check(7)(initial = 4)
      check(9)(initial = 4)
      check(40)(initial = 1, leavesSize = PositiveSize(5), nodeSize = 5)

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

//      sample.tree match
//        case null => 0
//        case Single(elem) => elem
//        case Multiple(elems, parent) => elems.length
//        case Node(elems, parent) => elems.length

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
