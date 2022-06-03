package scalax.collection

import scala.collection.mutable.ListBuffer

import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.GraphTraversal._
import scalax.collection.generic.GraphCoreCompanion
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
final class Traversal[G[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, G]](val factory: GraphCoreCompanion[G])
    extends RefSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with Visualizer[G] {

  implicit val config = PropertyCheckConfiguration(minSuccessful = 5, maxDiscardedFactor = 1.0)

  val predecessors = Parameters(direction = Predecessors)
  val anyConnected = Parameters(direction = AnyConnected)

  def `find successors in a tiny graph`: Unit =
    given(factory(1 ~> 2)) { g =>
      val (n1, n2) = (g get 1, g get 2)

      var successor = n1 findSuccessor (_ == 1)
      successor shouldBe empty

      successor = n1 findSuccessor (_ == 3)
      successor shouldBe empty

      successor = n2 findSuccessor (_ == 1)
      successor shouldBe empty

      successor = n1 findSuccessor (_ == 2)
      successor shouldBe defined
      successor.get should be(n2)
    }

  def `find predecessors in a tiny graph`: Unit =
    given(factory(1 ~> 2)) { g =>
      val (n1, n2) = (g get 1, g get 2)

      var predecessor = n1 findPredecessor (_ == 1)
      predecessor shouldBe empty

      predecessor = n1 findPredecessor (_ == 3)
      predecessor shouldBe empty

      predecessor = n1 findPredecessor (_ == 2)
      predecessor shouldBe empty

      predecessor = n2 findPredecessor (_ == 1)
      predecessor shouldBe defined
      predecessor.get should be(n1)
    }

  def `find connected nodes by predicate in a tiny graph`: Unit =
    given(factory(1 ~> 2)) { g =>
      val (n1, n2) = (g get 1, g get 2)

      var connected = n1 findConnected (_ == 1)
      connected shouldBe empty

      connected = n1 findConnected (_ == 3)
      connected shouldBe empty

      connected = n1 findConnected (_ == 2)
      connected shouldBe defined
      connected.get should be(n2)

      connected = n2 findConnected (_ == 1)
      connected shouldBe defined
      connected.get should be(n1)
    }

  import Data._
  object Di_1   extends TGraph(factory.from(elementsOfDi_1))
  object UnDi_1 extends TGraph(factory.from(elementsOfMixed_1))

  def `find successors in a mid-size graph`: Unit = {
    val g             = Di_1
    def n(outer: Int) = g.node(outer)
    var successor     = null.asInstanceOf[Option[g.g.NodeT]]

    given(g.g) { _ =>
      successor = n(3) findSuccessor (_ == 0)
      successor shouldBe empty

      successor = n(3) findSuccessor (_ == 3)
      successor shouldBe empty

      successor = n(3) findSuccessor (_ == 7)
      successor shouldBe empty

      successor = n(2) findSuccessor (_ == 5)
      successor shouldBe defined
      successor.get should be(5)

      successor = n(3) findSuccessor (_ > 4)
      successor shouldBe defined
      successor.get should be(5)
    }
  }

  def `find predecessors in a mid-size graph`: Unit = {
    val g             = Di_1
    def n(outer: Int) = g.node(outer)
    var predecessor   = null.asInstanceOf[Option[g.g.NodeT]]

    given(g.g) { _ =>
      predecessor = n(3) findPredecessor (_ == 0)
      predecessor shouldBe empty

      predecessor = n(3) findPredecessor (_ == 3)
      predecessor shouldBe empty

      predecessor = n(3) findPredecessor (_ == 5)
      predecessor shouldBe empty

      predecessor = n(3) findPredecessor (_ == 4)
      predecessor shouldBe defined
      predecessor.get should be(4)

      predecessor = n(3) findPredecessor (_ > 2)
      predecessor shouldBe defined
      predecessor.get should be(4)
    }
  }

  def `find connected nodes by predicate in a mid-size graph`: Unit = {
    val g             = Di_1
    def n(outer: Int) = g.node(outer)
    var connected     = null.asInstanceOf[Option[g.g.NodeT]]

    given(g.g) { _ =>
      connected = n(3) findConnected (_ == 0)
      connected shouldBe empty

      connected = n(3) findConnected (_ == 3)
      connected shouldBe empty

      connected = n(2) findConnected (_ == 4)
      connected shouldBe defined
      connected.get should be(4)

      connected = n(3) findConnected (_ > 3)
      connected shouldBe defined
      connected.get should (be(4) or be(5))
    }
  }

  def `find path to a successor in a tiny graph`: Unit =
    given(factory(1, 2 ~ 3, 3 ~ 4, 5 ~ 6, 6 ~ 1)) { g =>
      val n1 = g get 1
      n1 pathUntil (_ == n1) should be(None)

      val n2 = g get 2
      n2 pathUntil (_ == n1) should be(None)

      val n5       = g get 5
      val n6       = g get 6
      val expected = List(n5, n6, n1)
      val r5       = n5 pathUntil (_ < 4)
      r5 shouldBe defined
      val p5 = r5.get
      p5.nodes.toList should be(expected)

      p5.size should be(expected.size + (expected.size - 1))
      p5.length should be(expected.size - 1)
    }

  def `find path to a successor`: Unit =
    given(factory(0 ~ 1, 1 ~ 2)) { g =>
      def n(outer: Int) = g get outer
      for (i <- 0 to 2)
        (n(0) pathTo n(i)).get.length should be(i)
    }

  def `assert fix_110409 of shortestPathTo`: Unit =
    given(factory(0 ~ 1, 1 ~ 2, 2 ~ 3)) { g =>
      def n(outer: Int) = g get outer
      (n(0) shortestPathTo n(0)).get.length should be(0)
      (n(0) shortestPathTo n(3)).get.nodes.toList should be(List(0, 1, 2, 3))
      (n(1) shortestPathTo n(3)).get.nodes.toList should be(List(1, 2, 3))
    }

  def `traverser to graph`: Unit =
    given(Di_1.g) { g =>
      def innerNode(outer: Int) = g get outer

      innerNode(1).outerNodeTraverser.toGraph should equal(factory(1 ~> 2, 2 ~> 3, 3 ~> 5, 1 ~> 5, 1 ~> 3))

      innerNode(2).outerNodeTraverser(anyConnected).toGraph should equal(
        factory(1 ~> 2, 2 ~> 3, 4 ~> 3, 3 ~> 5, 1 ~> 5, 1 ~> 3)
      )

      innerNode(3).outerNodeTraverser(predecessors).toGraph should equal(factory(4 ~> 3, 1 ~> 3, 2 ~> 3, 1 ~> 2))
    }

  def `traverser with an extended visitor`: Unit = {
    import UnDi_1.g.ExtendedNodeVisitor
    def n(outer: Int) = UnDi_1.node(outer)

    given(UnDi_1.g) { _ =>
      var lastCount = 0
      n(1).innerNodeTraverser.withKind(DepthFirst) foreach
        ExtendedNodeVisitor { (node, count, depth, informer) =>
          count should be(lastCount + 1)
          lastCount += 1

          node.outer match {
            case 1 => depth should be(0)
            case 2 => depth should (be(1) or be(2) or be(3))
            case 3 => depth should (be(1) or be(2))
            case 4 => depth should (be(2) or be(3))
            case 5 => depth should (be > 0 and be < 5)
          }
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
    {
      import UnDi_1._
      given(UnDi_1.g) { _ =>
        val bfs_4 = node(4).outerNodeTraverser
        bfs_4.sum should be(expectedSumAll)
        bfs_4.withMaxDepth(1).sum should be(expectedSumLayer1)
        bfs_4.withMaxDepth(2).sum should be(expectedSumLayer2)

        val dfs_4 = bfs_4.withKind(DepthFirst)
        dfs_4.withMaxDepth(1).sum should be(expectedSumLayer1)
        dfs_4.withMaxDepth(2).sum should be(expectedSumLayer2)

        val sub_4 = bfs_4.withSubgraph(nodes = _ <= 4)
        sub_4.sum should be(expectedSumAllExclGt4)
        sub_4.withMaxDepth(2).sum should be(expectedSumLayer2ExclGt4)
        sub_4.withKind(DepthFirst).sum should be(expectedSumAllExclGt4)
      }
    }
  }

  def `DownUp traverser`: Unit =
    given(Di_1.g) { g =>
      def innerNode(outer: Int) = g get outer
      var stack                 = List.empty[Int]

      innerNode(4).innerNodeDownUpTraverser foreach { case (down, node) =>
        if (down) stack = node.outer +: stack
        else {
          stack.head should be(node.outer)
          stack = stack.tail
        }
      }
      stack shouldBe empty
    }

  def `DownUp traverser for computing braces`: Unit = {
    val root = "A"
    given(factory(root ~> "B1", root ~> "B2")) { g =>
      val innerRoot = g get root
      val result = innerRoot.innerNodeDownUpTraverser.foldLeft(ListBuffer.empty[String]) { (buf, param) =>
        param match {
          case (down, node) =>
            if (down) buf += (if (node eq innerRoot) "(" else "[") += node.toString
            else buf += (if (node eq innerRoot) ")" else "]")
        }
      }
      result.foldLeft("")(_ + _) should (be("(A[B1][B2])") or
        be("(A[B2][B1])"))
    }
  }

  abstract class Elem(val name: String) {
    def balance: Int
  }

  def `DownUp traverser for computing sums`: Unit = {
    case class Node(override val name: String) extends Elem(name) {
      var sum: Int = 0
      def balance  = sum
    }
    case class Leaf(override val name: String, override val balance: Int) extends Elem(name)
    val root          = Node("R")
    val (nA, nB, nBA) = (Node("A"), Node("B"), Node("BA"))

    given(
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
      )
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
          case n: Node => n.balance should be(expected(n))
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

      val expectedSumLayer1SuccessorsOf_2    = 5
      val expectedSumLayer1PredecessorsOf_2  = 3
      val expectedSumLayer1AnyConnectedsOf_2 = 6
    }
    {
      import DDi_1._
      given(DDi_1.g) { _ =>
        val maxDepth_1 = Parameters(maxDepth = 1)

        node(4).outerNodeTraverser.sum should be(expectedSumSuccessorsOf_4)
        node(4).outerNodeTraverser(predecessors).sum should be(expectedSumPredecessorsOf_4)

        node(2).outerNodeTraverser.sum should be(expectedSumSuccessorsOf_2)
        node(2).outerNodeTraverser(predecessors).sum should be(expectedSumPredecessorsOf_2)
        node(2).outerNodeTraverser(anyConnected).sum should be(expectedSumAnyConnected)

        node(2).outerNodeTraverser(maxDepth_1).sum should be(expectedSumLayer1SuccessorsOf_2)
        node(2).outerNodeTraverser(maxDepth_1.withDirection(Predecessors)).sum should be(
          expectedSumLayer1PredecessorsOf_2
        )
        node(2).outerNodeTraverser(maxDepth_1.withDirection(AnyConnected)).sum should be(
          expectedSumLayer1AnyConnectedsOf_2
        )
        an[IllegalArgumentException] should be thrownBy {
          node(2).innerNodeTraverser(anyConnected) pathTo node(2)
        }
        an[IllegalArgumentException] should be thrownBy {
          node(2).innerNodeTraverser(anyConnected) shortestPathTo node(2)
        }
      }
    }
  }

  def `traverser withOrdering for nodes`: Unit =
    given(
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
      orderedTraverser.toList should be(
        List(0 to 4: _*) ++
          List(11 to 13: _*) ++ List(21 to 23: _*) ++
          List(31 to 33: _*) ++ List(41 to 43: _*)
      )

      orderedTraverser.withKind(DepthFirst).toList should be(
        0 ::
          List(1) ::: List(11 to 13: _*) ::: List(2) ::: List(21 to 23: _*) :::
          List(3) ::: List(31 to 33: _*) ::: List(4) ::: List(41 to 43: _*)
      )
    }

  def `map Traverser result`: Unit =
    given(Di_1.g) { _ =>
      val t = Di_1.g.nodes.head.outerNodeTraverser
      t map (_ + 1) should be(t.toList map (_ + 1))
    }

  def `traverser for inner elements`: Unit = {
    import Di_1._
    import g.{InnerEdge, InnerNode}

    given(Di_1.g) { _ =>
      val t                    = g.nodes.head.innerElemTraverser
      def nodePred(n: g.NodeT) = n.degree > 1
      def edgePred(e: g.EdgeT) = e.ends forall nodePred

      val nodes = t collect { case n: InnerNode if nodePred(n.asNodeT) => n.asNodeT }
      val edges = t collect { case e: InnerEdge if edgePred(e.asEdgeT) => e.asEdgeT }
      nodes.toSet should be(g.nodes filter nodePred)
      edges.toSet should be(g.edges filter edgePred)
    }
  }
}
