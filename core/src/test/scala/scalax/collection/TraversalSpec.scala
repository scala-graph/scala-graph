package scalax.collection

import scala.collection.mutable.ListBuffer

import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scalax.collection.OuterImplicits._
import scalax.collection.GraphTraversal._
import scalax.collection.edges._
import scalax.collection.generic.{Edge, GenericGraphCoreFactory}
import scalax.collection.visualization.Visualizer

class TraversalSpec
    extends Suites(
      new Traversal[immutable.Graph](immutable.Graph),
      new Traversal[mutable.Graph](mutable.Graph)
    )

/** This class contains tests for graph traversals to be run for Graph instances created
  * by the Graph factory and passed to the constructor.
  * It allows the same tests to be run for mutable and immutable Graphs.
  */
final private class Traversal[G[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, G]](
    val factory: GenericGraphCoreFactory[G]
) extends RefSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with IntelliJ[G]
    with Visualizer {

  implicit val config: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 5, maxDiscardedFactor = 1.0)

  val predecessors = Parameters(direction = Predecessors)
  val anyConnected = Parameters(direction = AnyConnected)

  def `find successors in a tiny graph`: Unit =
    withGraph(factory(1 ~> 2).asAnyGraph) { g =>
      val (n1, n2) = (g get 1, g get 2)

      List(1, 3) foreach { i =>
        n1 findSuccessor (_.outer == i) shouldBe empty
      }
      n2 findSuccessor (_.outer == 1) shouldBe empty
      n1 findSuccessor (_.outer == 2) shouldBe Some(n2)
    }

  def `find predecessors in a tiny graph`: Unit =
    withGraph(factory(1 ~> 2)) { g =>
      val (n1, n2) = (g get 1, g get 2)

      1 to 3 foreach { i =>
        n1 findPredecessor (_.outer == i) shouldBe empty
      }
      val predecessor = n2 findPredecessor (_.outer == 1)
      predecessor shouldBe Some(n1)
    }

  def `find connected nodes by predicate in a tiny graph`: Unit =
    withGraph(factory(1 ~> 2)) { g =>
      val (n1, n2) = (g get 1, g get 2)

      List(1, 3) foreach { i =>
        n1 findConnected (_.outer == i) shouldBe empty
      }
      n1 findConnected (_.outer == 2) shouldBe Some(n2)
      n2 findConnected (_.outer == 1) shouldBe Some(n1)
    }

  import Data._
  object Di_1   extends TGraph(factory.from(elementsOfDi_1))
  object UnDi_1 extends TGraph(factory.from(elementsOfMixed_1))

  def `find successors in a mid-size graph`: Unit =
    withGraph(Di_1.g.asAnyGraph) { g =>
      def n(outer: Int) = g.get(outer)

      List(0, 3, 7) foreach { i =>
        n(3) findSuccessor (_.outer == i) shouldBe empty
      }
      n(2) findSuccessor (_.outer == 5) shouldBe Some(5)
      n(3) findSuccessor (_.outer > 4) shouldBe Some(5)
    }

  def `find predecessors in a mid-size graph`: Unit =
    withGraph(Di_1.g.asAnyGraph) { g =>
      def n(outer: Int) = g.get(outer)

      List(0, 3, 5) foreach { i =>
        n(3) findPredecessor (_.outer == i) shouldBe empty
      }
      n(3) findPredecessor (_.outer == 4) shouldBe Some(4)
      n(3) findPredecessor (_ > 2) shouldBe Some(4)
    }

  def `find connected nodes by predicate`: Unit =
    withGraph(Di_1.g.asAnyGraph) { g =>
      def n(outer: Int) = g get outer

      List(0, 3) foreach { i =>
        n(3) findConnected (_.outer == i) shouldBe empty
      }
      n(2) findConnected (_.outer == 4) shouldBe Some(4)
      n(3) findConnected (_.outer > 3) should (be(Some(4)) or be(Some(5)))
    }

  def `find path to a successor`: Unit =
    withGraph(factory(1, 2 ~ 3, 3 ~ 4, 5 ~ 6, 6 ~ 1)) { g =>
      val n1 = g get 1
      n1 pathUntil (_ == n1) shouldBe None

      val n2 = g get 2
      n2 pathUntil (_ == n1) shouldBe None

      val n5       = g get 5
      val n6       = g get 6
      val expected = List(n5, n6, n1)
      val r5       = n5 pathUntil (_ < 4)
      r5 shouldBe defined
      val p5 = r5.get
      p5.nodes.toList shouldBe expected

      p5.size shouldBe (expected.size + (expected.size - 1))
      p5.length shouldBe (expected.size - 1)
    }

  def `find path to a successor in a minimalistic graph`: Unit =
    withGraph(factory(0 ~ 1, 1 ~ 2)) { g =>
      def n(outer: Int) = g get outer
      for (i <- 0 to 2)
        (n(0) pathTo n(i)).get.length shouldBe i
    }

  def `assert fix_110409 of shortestPathTo`: Unit =
    withGraph(factory(0 ~ 1, 1 ~ 2, 2 ~ 3)) { g =>
      def n(outer: Int) = g get outer
      (n(0) shortestPathTo n(0)).get.length shouldBe 0
      (n(0) shortestPathTo n(3)).get.nodes.toList shouldBe List(0, 1, 2, 3)
      (n(1) shortestPathTo n(3)).get.nodes.toList shouldBe List(1, 2, 3)
    }

  def `traverser to graph`: Unit =
    withGraph(Di_1.g.asAnyGraph) { g =>
      def innerNode(outer: Int) = g get outer

      innerNode(1).outerNodeTraverser.to(factory) should equal(factory(1 ~> 2, 2 ~> 3, 3 ~> 5, 1 ~> 5, 1 ~> 3))

      innerNode(2).outerNodeTraverser(anyConnected).to(factory) should equal(
        factory(1 ~> 2, 2 ~> 3, 4 ~> 3, 3 ~> 5, 1 ~> 5, 1 ~> 3)
      )

      innerNode(3).outerNodeTraverser(predecessors).to(factory) should equal(factory(4 ~> 3, 1 ~> 3, 2 ~> 3, 1 ~> 2))
    }

  def `traverser with an extended visitor`: Unit =
    withGraph(UnDi_1.g.asAnyGraph) { g =>
      import g.ExtendedNodeVisitor
      def n(outer: Int) = g get outer

      var lastCount = 0
      n(1).innerNodeTraverser.withKind(DepthFirst) foreach
        ExtendedNodeVisitor { (node, count, depth, _) =>
          count shouldBe (lastCount + 1)
          lastCount += 1

          node.outer match {
            case 1 => depth shouldBe 0
            case 2 => depth should (be(1) or be(2) or be(3))
            case 3 => depth should (be(1) or be(2))
            case 4 => depth should (be(2) or be(3))
            case 5 => depth should (be > 0 and be < 5)
          }
        }
    }

  def `traverser withMaxDepth`: Unit = {
    import Data._
    object UnDi_1 extends TGraph(factory.from(elementsOfMixed_1)) {
      val expectedSumAll           = 15
      val expectedSumLayer1        = 12
      val expectedSumLayer2        = 15
      val expectedSumAllExclGt4    = 10
      val expectedSumLayer2ExclGt4 = 9
    }
    import UnDi_1._
    withGraph(g.asAnyGraph) { g =>
      def n(outer: Int) = g get outer

      val bfs_4 = n(4).outerNodeTraverser
      bfs_4.sum shouldBe expectedSumAll
      bfs_4.withMaxDepth(1).sum shouldBe expectedSumLayer1
      bfs_4.withMaxDepth(2).sum shouldBe expectedSumLayer2

      val dfs_4 = bfs_4.withKind(DepthFirst)
      dfs_4.withMaxDepth(1).sum shouldBe expectedSumLayer1
      dfs_4.withMaxDepth(2).sum shouldBe expectedSumLayer2

      val sub_4 = bfs_4.withSubgraph(nodes = _ <= 4)
      sub_4.sum shouldBe expectedSumAllExclGt4
      sub_4.withMaxDepth(2).sum shouldBe expectedSumLayer2ExclGt4
      sub_4.withKind(DepthFirst).sum shouldBe expectedSumAllExclGt4
    }
  }

  def `DownUp traverser`: Unit =
    withGraph(Di_1.g.asAnyGraph) { g =>
      def innerNode(outer: Int) = g get outer
      var stack                 = List.empty[Int]

      innerNode(4).innerNodeDownUpTraverser foreach { case (down, node) =>
        if (down) stack = node.outer +: stack
        else {
          stack.head shouldBe node.outer
          stack = stack.tail
        }
      }
      stack shouldBe empty
    }

  def `DownUp traverser for computing braces`: Unit = {
    val root = "A"
    withGraph(factory(root ~> "B1", root ~> "B2")) { g =>
      val innerRoot = g get root
      val result = innerRoot.innerNodeDownUpTraverser.foldLeft(ListBuffer.empty[String]) { (buf, param) =>
        param match {
          case (down, node) =>
            if (down) buf += (if (node eq innerRoot) "(" else "[") += node.toString
            else buf += (if (node eq innerRoot) ")" else "]")
        }
      }
      result.foldLeft("")(_ + _) should (
        be("(A[B1][B2])") or
          be("(A[B2][B1])")
      )
    }
  }

  def `DownUp traverser for computing sums`: Unit = {
    abstract class Elem(val name: String) {
      def balance: Int
    }
    case class Node(override val name: String) extends Elem(name) {
      var sum: Int = 0
      def balance  = sum
    }
    case class Leaf(override val name: String, override val balance: Int) extends Elem(name)

    val root          = Node("R")
    val (nA, nB, nBA) = (Node("A"), Node("B"), Node("BA"))

    withGraph(
      factory[Elem, DiEdge](
        root ~> nA,
        root ~> nB,
        nA ~> Leaf("LA1", 1),
        nA ~> Leaf("LA2", 2),
        nB ~> Leaf("B1", 3),
        nB ~> nBA,
        nBA ~> Leaf("BA1", 10),
        nBA ~> Leaf("BA2", 11),
        nBA ~> Leaf("BA3", 12)
      ).asAnyGraph
    ) { g =>
      (g get root).innerNodeDownUpTraverser foreach { case (down, node) =>
        if (!down)
          node.outer match {
            case n: Node => n.sum = node.diSuccessors.foldLeft(0)(_ + _.balance)
            case _       =>
          }
      }
      val expected = Map(root -> 39, nA -> 3, nB -> 36, nBA -> 33)
      g.nodes foreach {
        _.outer match {
          case n: Node => n.balance shouldBe (expected(n))
          case _       =>
        }
      }
    }
  }

  def `traverser withDirection`: Unit = {
    // https://groups.google.com/forum/?fromgroups=#!topic/scala-internals/9NMPfU4xdhU
    object DDi_1 extends TGraph(factory.from(elementsOfDi_1)) {
      val expectedSumSuccessorsOf_4   = 12
      val expectedSumPredecessorsOf_4 = 4
      val expectedSumSuccessorsOf_2   = 10
      val expectedSumPredecessorsOf_2 = 3
      val expectedSumAnyConnected     = 15

      val expectedSumLayer1SuccessorsOf_2     = 5
      val expectedSumLayer1PredecessorsOf_2   = 3
      val expectedSumLayer1AnyConnectedWith_2 = 6
    }
    import DDi_1._
    withGraph(DDi_1.g.asAnyGraph) { g =>
      def n(outer: Int) = g get outer

      val maxDepth_1 = Parameters(maxDepth = 1)

      n(4).outerNodeTraverser.sum shouldBe expectedSumSuccessorsOf_4
      n(4).outerNodeTraverser(predecessors).sum shouldBe expectedSumPredecessorsOf_4

      n(2).outerNodeTraverser.sum shouldBe expectedSumSuccessorsOf_2
      n(2).outerNodeTraverser(predecessors).sum shouldBe expectedSumPredecessorsOf_2
      n(2).outerNodeTraverser(anyConnected).sum shouldBe expectedSumAnyConnected

      n(2).outerNodeTraverser(maxDepth_1).sum shouldBe expectedSumLayer1SuccessorsOf_2
      n(2).outerNodeTraverser(maxDepth_1.withDirection(Predecessors)).sum shouldBe expectedSumLayer1PredecessorsOf_2

      n(2).outerNodeTraverser(maxDepth_1.withDirection(AnyConnected)).sum shouldBe
        expectedSumLayer1AnyConnectedWith_2

      an[IllegalArgumentException] should be thrownBy {
        n(2).innerNodeTraverser(anyConnected) pathTo n(2)
      }
      an[IllegalArgumentException] should be thrownBy {
        n(2).innerNodeTraverser(anyConnected) shortestPathTo n(2)
      }
    }
  }

  def `traverser withOrdering for nodes`: Unit =
    withGraph(
      factory(
        0 ~> 4,
        0 ~> 2,
        0 ~> 3,
        0 ~> 1,
        1 ~> 13,
        1 ~> 11,
        1 ~> 12,
        2 ~> 22,
        2 ~> 21,
        2 ~> 23,
        3 ~> 32,
        3 ~> 33,
        3 ~> 31,
        4 ~> 42,
        4 ~> 41,
        4 ~> 43
      )
    ) { g =>
      val root         = g get 0
      val nodeOrdering = g.NodeOrdering(Ordering.Int.compare(_, _))

      val orderedTraverser = root.outerNodeTraverser.withOrdering(nodeOrdering)
      orderedTraverser.toList shouldBe (
        List(0 to 4: _*) ++
          List(11 to 13: _*) ++ List(21 to 23: _*) ++
          List(31 to 33: _*) ++ List(41 to 43: _*)
      )

      orderedTraverser.withKind(DepthFirst).toList shouldBe (
        0 ::
          List(1) ::: List(11 to 13: _*) ::: List(2) ::: List(21 to 23: _*) :::
          List(3) ::: List(31 to 33: _*) ::: List(4) ::: List(41 to 43: _*)
      )
    }

  def `map Traverser result`: Unit =
    withGraph(Di_1.g.asAnyGraph) { g =>
      val t = g.nodes.head.outerNodeTraverser
      t map (_ + 1) shouldBe (t.toList map (_ + 1))
    }

  def `traverser for inner elements`: Unit =
    withGraph(Di_1.g.asAnyGraph) { g =>
      val t = g.nodes.head.innerElemTraverser

      def nodePred(n: g.NodeT) = n.degree > 1
      def edgePred(e: g.EdgeT) = e.ends forall nodePred

      val nodes = t collect { case g.InnerNode(n, _) if nodePred(n) => n }
      val edges = t collect { case g.InnerEdge(e, _) if edgePred(e) => e }

      nodes.toSet shouldBe (g.nodes filter nodePred)
      edges.toSet shouldBe (g.edges filter edgePred)
    }
}
