package scalax.collection

import scala.language.{higherKinds, postfixOps}
import scala.collection.mutable.ListBuffer
import scala.util.Random
import GraphPredef._
import GraphEdge._
import GraphTraversal._
import generic.GraphCoreCompanion
import edge.WDiEdge
import edge.WUnDiEdge
import edge.Implicits._
import generator.GraphGen
import org.scalacheck._
import Arbitrary.arbitrary
import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scalax.collection.visualization.Visualizer

@RunWith(classOf[JUnitRunner])
class TTraversalRootTest
    extends Suites(
      new TTraversal[immutable.Graph](immutable.Graph),
      new TTraversal[mutable.Graph](mutable.Graph)
    )

/**	This class contains tests for graph traversals to be run for Graph instances created
  *	by the Graph factory and passed to the constructor. For instance,
  *	this allows the same tests to be run for mutable and immutable Graphs.
  */
final class TTraversal[G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]](
    val factory: GraphCoreCompanion[G])
    extends RefSpec
    with Matchers
    with PropertyChecks
    with Visualizer[G] {

  implicit val config = PropertyCheckConfiguration(minSuccessful = 5, maxDiscardedFactor = 1.0)

  def `find successors in a tiny graph` {
    given(factory(1 ~> 2)) { g =>
      val (n1, n2) = (g get 1, g get 2)

      var successor = n1 findSuccessor (_ == 1)
      successor should be('isEmpty)

      successor = n1 findSuccessor (_ == 3)
      successor should be('isEmpty)

      successor = n2 findSuccessor (_ == 1)
      successor should be('isEmpty)

      successor = n1 findSuccessor (_ == 2)
      successor should be('isDefined)
      successor.get should be(n2)
    }
  }

  def `find predecessors in a tiny graph` {
    given(factory(1 ~> 2)) { g =>
      val (n1, n2) = (g get 1, g get 2)

      var predecessor = n1 findPredecessor (_ == 1)
      predecessor should be('isEmpty)

      predecessor = n1 findPredecessor (_ == 3)
      predecessor should be('isEmpty)

      predecessor = n1 findPredecessor (_ == 2)
      predecessor should be('isEmpty)

      predecessor = n2 findPredecessor (_ == 1)
      predecessor should be('isDefined)
      predecessor.get should be(n1)
    }
  }

  def `find connected nodes by predicate in a tiny graph` {
    given(factory(1 ~> 2)) { g =>
      val (n1, n2) = (g get 1, g get 2)

      var connected = n1 findConnected (_ == 1)
      connected should be('isEmpty)

      connected = n1 findConnected (_ == 3)
      connected should be('isEmpty)

      connected = n1 findConnected (_ == 2)
      connected should be('isDefined)
      connected.get should be(n2)

      connected = n2 findConnected (_ == 1)
      connected should be('isDefined)
      connected.get should be(n1)
    }
  }

  import Data._
  object Di_1   extends TGraph(factory(elementsOfDi_1: _*))
  object UnDi_1 extends TGraph(factory(elementsOfUnDi_1: _*))

  def `find successors in a mid-size graph` {
    val g             = Di_1
    def n(outer: Int) = g.node(outer)
    var successor     = null.asInstanceOf[Option[g.g.NodeT]]

    given(g.g) { _ =>
      successor = n(3) findSuccessor (_ == 0)
      successor should be('isEmpty)

      successor = n(3) findSuccessor (_ == 3)
      successor should be('isEmpty)

      successor = n(3) findSuccessor (_ == 7)
      successor should be('isEmpty)

      successor = n(2) findSuccessor (_ == 5)
      successor should be('isDefined)
      successor.get should be(5)

      successor = n(3) findSuccessor (_ > 4)
      successor should be('isDefined)
      successor.get should be(5)
    }
  }

  def `find predecessors in a mid-size graph` {
    val g             = Di_1
    def n(outer: Int) = g.node(outer)
    var predecessor   = null.asInstanceOf[Option[g.g.NodeT]]

    given(g.g) { _ =>
      predecessor = n(3) findPredecessor (_ == 0)
      predecessor should be('isEmpty)

      predecessor = n(3) findPredecessor (_ == 3)
      predecessor should be('isEmpty)

      predecessor = n(3) findPredecessor (_ == 5)
      predecessor should be('isEmpty)

      predecessor = n(3) findPredecessor (_ == 4)
      predecessor should be('isDefined)
      predecessor.get should be(4)

      predecessor = n(3) findPredecessor (_ > 2)
      predecessor should be('isDefined)
      predecessor.get should be(4)
    }
  }

  def `find connected nodes by predicate in a mid-size graph` {
    val g             = Di_1
    def n(outer: Int) = g.node(outer)
    var connected     = null.asInstanceOf[Option[g.g.NodeT]]

    given(g.g) { _ =>
      connected = n(3) findConnected (_ == 0)
      connected should be('isEmpty)

      connected = n(3) findConnected (_ == 3)
      connected should be('isEmpty)

      connected = n(2) findConnected (_ == 4)
      connected should be('isDefined)
      connected.get should be(4)

      connected = n(3) findConnected (_ > 3)
      connected should be('isDefined)
      connected.get should (be(4) or be(5))
    }
  }

  def `find path to a successor in a tiny graph` {
    given(factory(1, 2 ~ 3, 3 ~ 4, 5 ~ 6, 6 ~ 1)) { g =>
      val n1 = g get 1
      n1 pathUntil (_ == n1) should be(None)

      val n2 = g get 2
      n2 pathUntil (_ == n1) should be(None)

      val n5       = g get 5
      val n6       = g get 6
      val expected = List(n5, n6, n1)
      val r5       = n5 pathUntil (_ < 4)
      r5 should be('isDefined)
      val p5 = r5.get
      p5.nodes.toList should be(expected)

      p5.size should be(expected.size + (expected.size - 1))
      p5.length should be(expected.size - 1)
    }
  }

  def `find path to a successor` {
    given(factory(0 ~ 1, 1 ~ 2)) { g =>
      def n(outer: Int) = g get outer
      for (i <- 0 to 2)
        (n(0) pathTo n(i)).get.length should be(i)
    }
  }

  def `assert fix_110409 of shortestPathTo` {
    given(factory(0 ~ 1, 1 ~ 2, 2 ~ 3)) { g =>
      def n(outer: Int) = g get outer
      (n(0) shortestPathTo n(0)).get.length should be(0)
      (n(0) shortestPathTo n(3)).get.nodes.toList should be(List(0, 1, 2, 3))
      (n(1) shortestPathTo n(3)).get.nodes.toList should be(List(1, 2, 3))
    }
  }

  def `assert bug 9 of shortestPathTo is fixed` {
    given(factory(0 ~> 1 % 3, 0 ~> 2 % 4, 1 ~> 3 % 3, 2 ~> 3 % 1)) { g =>
      def n(outer: Int) = g get outer
      (n(0) shortestPathTo n(3)).get.nodes.toList should be(List(0, 2, 3))
    }
  }

  def `shortestPathTo in WDi_1` {
    given(factory(elementsOfWDi_1: _*)) { g =>
      def n(outer: Int) = g get outer

      n(5) shortestPathTo n(4) should be(None)
      n(5) shortestPathTo n(1) should be(None)
      n(3) shortestPathTo n(1) should be(None)

      (n(1) shortestPathTo n(3)).get.nodes.toList should be(List(1, 3))
      (n(4) shortestPathTo n(5)).get.nodes.toList should be(List(4, 3, 5))
      (n(1) shortestPathTo n(5)).get.nodes.toList should be(List(1, 5))
    }
  }

  def `shortestPathTo in WDi_1 using Float` {
    given(factory(elementsOfWDi_1: _*)) { g =>
      def n(outer: Int) = g get outer

      def weight(e: g.EdgeT): Float         = 0.5f + e.weight.toFloat
      def reverseWeight(e: g.EdgeT): Double = 41 - e.weight

      n(5) shortestPathTo (n(4), weight) shouldBe empty

      (n(1) shortestPathTo (n(3), weight)).get.nodes.toStream should contain theSameElementsInOrderAs Array(1, 3)
      (n(1) shortestPathTo (n(3), reverseWeight)).get.nodes.toStream should contain theSameElementsInOrderAs Array(
        1,
        2,
        3)
    }
  }

  def `shortestPathTo in WUnDi_1` {
    given(factory(elementsOfWUnDi_1: _*)) { g =>
      def shortestPathNodes(from: Int, to: Int): Stream[g.NodeT] = {
        def n(value: Int): g.NodeT = g get value

        val path = n(from) shortestPathTo n(to)
        path shouldBe defined
        path.get.nodes.to[Stream]
      }
      shortestPathNodes(2, 5) should contain theSameElementsInOrderAs Array(2, 3, 4, 5)
      shortestPathNodes(4, 5) should contain theSameElementsInOrderAs Array(4, 5)
      shortestPathNodes(1, 3) should (contain theSameElementsInOrderAs (Array(1, 3)) or
        contain theSameElementsInOrderAs (Array(1, 5, 3)))
      shortestPathNodes(5, 4) should contain theSameElementsInOrderAs Array(5, 3, 4)
      shortestPathNodes(3, 1) should contain theSameElementsInOrderAs Array(3, 4, 5, 1)
    }
  }

  def `shortestPathTo withMaxDepth` {
    given(factory(elementsOfWUnDi_1: _*)) { g =>
      def n(value: Int): g.NodeT = g get value

      n(2).innerNodeTraverser.withMaxDepth(2).shortestPathTo(n(5)).get.nodes.toList should be(List(2, 3, 5))
    }
  }

  def `shortestPathTo withMaxWeight` {
    given(factory(elementsOfWUnDi_1: _*)) { g =>
      def n(value: Int): g.NodeT = g get value

      val t = n(2).innerNodeTraverser
      t.withMaxWeight(3).shortestPathTo(n(5)) shouldBe defined
      t.withMaxWeight(2).shortestPathTo(n(5)) shouldBe empty
    }
  }

  // see diagram WUnDi-2.jpg
  val eUnDi_2 = List[WUnDiEdge[Int]](1 ~ 2 % 4, 2 ~ 3 % -1, 1 ~> 3 % 5, 1 ~ 3 % 4, 1 ~> 2 % 3, 2 ~ 2 % 1)
  // 0        1         2         3        4         5
  val gUnDi_2 = factory.from[Int, WUnDiEdge](Set.empty, eUnDi_2)

  def `shortestPathTo in UnDi_2` {
    given(gUnDi_2) { g =>
      def n(value: Int) = g get value

      val p1_3 = n(1).shortestPathTo(n(3)).get
      p1_3.nodes.toList should be(List(1, 2, 3))
      p1_3.edges.toList should be(List(eUnDi_2(4), eUnDi_2(1)))

      val p2_1 = (n(2) shortestPathTo n(1)).get
      p2_1.nodes.toList should be(List(2, 3, 1))
      p2_1.edges.toList should be(List(eUnDi_2(1), eUnDi_2(3)))

      val p3_1 = (n(3) shortestPathTo n(1)).get
      p3_1.nodes.toList should be(List(3, 2, 1))
      p3_1.edges.toList should be(List(eUnDi_2(1), eUnDi_2(0)))

      val p3_3 = (n(3) shortestPathTo n(3)).get
      p3_3.nodes.toList should be(List(3))
      p3_3.edges.toList should be('empty)
    }
  }

  def `traverser withSubgraph` {
    given(gUnDi_2) { g =>
      def n(value: Int) = g get value

      val p2_1_nNE3 = n(2).withSubgraph(nodes = _ != 3).pathTo(n(1)).get
      p2_1_nNE3.nodes.toList should be(List(2, 1))
      p2_1_nNE3.edges.toList should be(List(2 ~ 1 % 4))

      val p1_3_wGT4 = n(1).withSubgraph(edges = _.weight > 4).pathTo(n(3)).get
      p1_3_wGT4.nodes.toList should be(List(1, 3))
      p1_3_wGT4.edges.toList should be(List(eUnDi_2(2)))

      val p1_3_wLT4 = n(1).withSubgraph(edges = _.weight < 4).pathTo(n(3)).get
      p1_3_wLT4.nodes.toList should be(List(1, 2, 3))
      p1_3_wLT4.edges.toList should be(List(eUnDi_2(4), eUnDi_2(1)))
    }
  }

  object `traverser withMaxWeight` {
    object WUnDi_1 extends TGraph[Int, WUnDiEdge, G](factory(elementsOfWUnDi_1: _*))
    import WUnDi_1._

    private def check(kind: Kind): Unit =
      List[Long](Long.MaxValue, 5, 4, 3, 2, 1, 0) map (max => n(1) withKind kind withMaxWeight max size) should be(
        List(5, 4, 3, 2, 1, 1, 1))

    def `calling DepthFirst` = given(WUnDi_1.g) { _ =>
      check(DepthFirst)
    }
    def `calling BreadthFirst` = given(WUnDi_1.g) { _ =>
      check(BreadthFirst)
    }
  }

  def `traverser with a visitor` {
    given(gUnDi_2) { g =>
      def n(value: Int) = g get value

      val nodes     = ListBuffer[g.NodeT]()
      val edges     = ListBuffer[g.EdgeT]()
      val traverser = n(2).innerElemTraverser.withSubgraph(nodes = _ != 3)
      traverser.pathTo(n(1)) {
        case g.InnerNode(n) => nodes += n
        case g.InnerEdge(e) => edges += e
      }

      nodes should be(List(n(2), n(1)))
      edges.toList.sorted(g.Edge.WeightOrdering) should be(List(eUnDi_2(1), eUnDi_2(5), eUnDi_2(0)))
    }
  }

  def `traverser with an extended visitor` {
    import UnDi_1.g.ExtendedNodeVisitor
    import UnDi_1.g.Informer.DfsInformer
    def n(outer: Int) = UnDi_1.node(outer)

    given(UnDi_1.g) { _ =>
      var lastCount = 0
      n(1).innerNodeTraverser.withKind(DepthFirst) foreach
        ExtendedNodeVisitor((node, count, depth, informer) => {
          count should be(lastCount + 1)
          lastCount += 1

          node.value match {
            case 1 => depth should be(0)
            case 2 => depth should (be(1) or (be(2) or be(3)))
            case 3 => depth should (be(1) or be(2))
            case 4 => depth should (be(2) or be(3))
            case 5 => depth should (be > (0) and be < (5))
          }
          informer match {
            case DfsInformer(stack, path) => ;
            case _                        => fail
          }
        })
    }
  }

  def `shortestPathTo in the flight example graph` {
    import custom.flight._, Helper._, Flight.ImplicitEdge
    val (jfc, lhr, dme, svx, fra, prg) =
      (Airport("JFC"), Airport("LHR"), Airport("DME"), Airport("SVX"), Airport("FRA"), Airport("PRG"))
    val flights: List[Flight[Airport]] =
      List(
        jfc ~> dme ## ("UN 2222", 14 o 25, 8 h 50),
        dme ~> svx ## ("UN 109", 23 o 10, 2 h 15),
        jfc ~> lhr ## ("BA 174", 19 o 10, 6 h 50),
        jfc ~> fra ## ("LH 400", 10 o 25, 8 h 20),
        jfc ~> fra ## ("UA 8840", 15 o 40, 7 h 35),
        lhr ~> dme ## ("BA 872", 8 o 55, 4 h 0),
        lhr ~> dme ## ("SU 242", 20 o 15, 3 h 50),
        lhr ~> fra ## ("LH 903", 9 o 50, 1 h 35),
        lhr ~> prg ## ("BA 860", 11 o 15, 2 h 0),
        fra ~> lhr ## ("LH 920", 19 o 50, 1 h 35),
        fra ~> dme ## ("LH 1444", 7 o 50, 3 h 10),
        fra ~> svx ## ("LH 1480", 19 o 20, 4 h 35),
        prg ~> svx ## ("U6 902", 21 o 55, 4 h 25)
      )
    def flight(flightNo: String) = flights find (_.flightNo == flightNo) get
    val g                        = factory.from[Airport, Flight](Set.empty, flights)
    given(g) { g =>
      val shp1 = (g get jfc).withSubgraph(edges = _.airline != "UN") shortestPathTo (g get dme)
      shp1.get.nodes.toList should be(List(jfc, lhr, dme))
      shp1.get.edges.toList should be(List(flight("BA 174"), flight("SU 242")))

      val shp2 = (g get lhr).withSubgraph(edges = _.airline != "SU") shortestPathTo (g get svx)
      shp2.get.edges.toList should be(List(flight("LH 903"), flight("LH 1480")))

      val shp3 = (g get dme).withSubgraph(nodes = _ != fra) shortestPathTo (g get jfc)
      shp3 should be(None)

      val shp4 = (g get jfc).withSubgraph(nodes = _ != dme) shortestPathTo (g get svx)
      shp4.get.nodes.toList should be(List(jfc, fra, svx))
      shp4.get.edges.toList should be(List(flight("UA 8840"), flight("LH 1480")))

      val visited = MSet[g.EdgeT]()
      (g get jfc).innerEdgeTraverser.shortestPathTo(g get lhr) { e: g.EdgeT =>
        visited += e
      }
      val visitedSorted = visited.toList.sortWith((a: g.EdgeT, b: g.EdgeT) => a.flightNo < b.flightNo)
      visitedSorted.sameElements(List(flight("BA 174"), flight("LH 400"), flight("UA 8840"), flight("UN 2222"))) should be(
        true)
    }
  }

  def `traverser withMaxDepth` {
    import Data._
    object UnDi_1 extends TGraph(factory(elementsOfUnDi_1: _*)) {
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

  def `DownUp traverser` {
    given(Di_1.g) { g =>
      def innerNode(outer: Int) = g get outer
      var stack                 = List.empty[Int]

      innerNode(4).innerNodeDownUpTraverser foreach (_ match {
        case (down, node) =>
          if (down) stack = node.value +: stack
          else {
            stack.head should be(node.value)
            stack = stack.tail
          }
      })
      stack should be('empty)
    }
  }

  def `DownUp traverser for computing braces` {
    val root = "A"
    given(factory(root ~> "B1", root ~> "B2")) { g =>
      val innerRoot = g get root
      val result = (ListBuffer.empty[String] /: innerRoot.innerNodeDownUpTraverser) { (buf, param) =>
        param match {
          case (down, node) =>
            if (down) buf += (if (node eq innerRoot) "(" else "[") += node.toString
            else buf += (if (node eq innerRoot) ")" else "]")
        }
      }
      ("" /: result)(_ + _) should (be("(A[B1][B2])") or
        be("(A[B2][B1])"))
    }
  }

  abstract class Elem(val name: String) {
    def balance: Int
  }

  def `DownUp traverser for computing sums` {
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
      )) { g =>
      (g get root).innerNodeDownUpTraverser foreach (_ match {
        case (down, node) =>
          if (!down)
            node.value match {
              case n: Node => n.sum = (0 /: node.diSuccessors)(_ + _.balance)
              case _       =>
            }
      })
      val expected = Map(root -> 39, nA -> 3, nB -> 36, nBA -> 33)
      g.nodes foreach {
        _.value match {
          case n: Node => n.balance should be(expected(n))
          case _       =>
        }
      }
    }
  }

  def `traverser withDirection` {
    // https://groups.google.com/forum/?fromgroups=#!topic/scala-internals/9NMPfU4xdhU
    object DDi_1 extends TGraph(factory(elementsOfDi_1: _*)) {
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
        val predecessors = Parameters(direction = Predecessors)
        val anyConnected = Parameters(direction = AnyConnected)
        val maxDepth_1   = Parameters(maxDepth = 1)

        node(4).outerNodeTraverser.sum should be(expectedSumSuccessorsOf_4)
        node(4).outerNodeTraverser(predecessors).sum should be(expectedSumPredecessorsOf_4)

        node(2).outerNodeTraverser.sum should be(expectedSumSuccessorsOf_2)
        node(2).outerNodeTraverser(predecessors).sum should be(expectedSumPredecessorsOf_2)
        node(2).outerNodeTraverser(anyConnected).sum should be(expectedSumAnyConnected)

        node(2).outerNodeTraverser(maxDepth_1).sum should be(expectedSumLayer1SuccessorsOf_2)
        node(2).outerNodeTraverser(maxDepth_1.withDirection(Predecessors)).sum should be(
          expectedSumLayer1PredecessorsOf_2)
        node(2).outerNodeTraverser(maxDepth_1.withDirection(AnyConnected)).sum should be(
          expectedSumLayer1AnyConnectedsOf_2)
        an[IllegalArgumentException] should be thrownBy {
          node(2).innerNodeTraverser(anyConnected) pathTo node(2)
        }
        an[IllegalArgumentException] should be thrownBy {
          node(2).innerNodeTraverser(anyConnected) shortestPathTo node(2)
        }
      }
    }
  }

  def `traverser withOrdering for nodes` {
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
        4 ~> 43)) { g =>
      val root         = g get 0
      val nodeOrdering = g.NodeOrdering(Ordering.Int.compare(_, _))

      val orderedTraverser = root.outerNodeTraverser.withOrdering(nodeOrdering)
      orderedTraverser.toList should be(
        List(0 to 4: _*) ++
          List(11 to 13: _*) ++ List(21 to 23: _*) ++
          List(31 to 33: _*) ++ List(41 to 43: _*))

      orderedTraverser.withKind(DepthFirst).toList should be(
        (0 ::
          List(1) ::: List(11 to 13: _*) ::: List(2) ::: List(21 to 23: _*) :::
          List(3) ::: List(31 to 33: _*) ::: List(4) ::: List(41 to 43: _*)))
    }
  }

  def `traverser withOrdering for edges` {
    val outerEdges = List(1 ~> 4 % 2, 1 ~> 2 % 5, 1 ~> 3 % 4, 3 ~> 6 % 4, 3 ~> 5 % 5, 3 ~> 7 % 2)
    given(factory(outerEdges: _*)) { g =>
      val root = g get 1

      def edgeOrdering = g EdgeOrdering (g.Edge.WeightOrdering.reverse.compare)

      val orderedTraverser = root.outerNodeTraverser.withOrdering(edgeOrdering)
      orderedTraverser.toList should be(List(1 to 7: _*))
      orderedTraverser.withKind(DepthFirst).toList should be(List(1, 2, 3, 5, 6, 7, 4))
    }
  }

  def `map Traverser result` {
    given(Di_1.g) { _ =>
      val t = Di_1.g.nodes.head.outerNodeTraverser
      t map (_ + 1) should be(t.toList map (_ + 1))
    }
  }

  def `traverser for inner elements` {
    import Di_1._
    import g.{InnerEdge, InnerNode}

    given(Di_1.g) { _ =>
      val t                    = g.nodes.head.innerElemTraverser
      def nodePred(n: g.NodeT) = n.degree > 1
      def edgePred(e: g.EdgeT) = e forall nodePred

      val nodes = t collect { case InnerNode(n) if nodePred(n) => n }
      val edges = t collect { case InnerEdge(e) if edgePred(e) => e }
      nodes.toSet should be(g.nodes filter nodePred)
      edges.toSet should be(g.edges filter edgePred)
    }
  }

  def `shortest path exists if path exists` {
    implicit val arbitraryWDiGraph = Arbitrary {
      import GraphGen.SmallInt._
      new GraphGen[Int, WDiEdge, G](factory, order, nodeGen, nodeDegrees, Set(WDiEdge), connected).apply
    }
    val r = new Random

    forAll(arbitrary[G[Int, WDiEdge]]) {
      given(_) { g =>
        def drawNode = g.nodes.draw(r)

        val (n1, n2)     = (drawNode, drawNode)
        val path         = n1 pathTo n2
        val shortestPath = n1 shortestPathTo n2

        path.isDefined should equal(shortestPath.isDefined)
      }
    }
  }
}
