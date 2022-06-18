package demo

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.{Graph, OuterElem, OuterNode}
import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.edges.labeled._
import scalax.collection.generic.AnyEdge

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-traversing.html
  * Traversing Graphs]].
  */
final class TraversingDemoSpec extends RefSpec with Matchers {

  type Outer = OuterElem[Int, AnyEdge[Int]]
  val g = Graph[Int, AnyEdge](1 ~ 2 % 4, 2 ~ 3 % 2, 1 ~> 3 % 5, 1 ~ 5 % 3, 3 ~ 5 % 2, 3 ~ 4 % 1, 4 ~> 4 % 1, 4 ~> 5 % 0)
  def n(outer: Int): g.NodeT = g get outer

  object `demonstrating ` {
    def `traversals for a result`: Unit = {
      def validatePath(p: g.Path, sample: List[Outer]): Unit = {
        def toN(p: Outer): Int = p match {
          case OuterNode(n) => n
          case _            => throw new IllegalArgumentException
        }
        p.isValid shouldBe true
        p.startNode == toN(sample.head) shouldBe true
        p.endNode == toN(sample.last) shouldBe true
      }

      n(1) findSuccessor (_.outDegree > 3) shouldBe None
      n(1) findSuccessor (_.outDegree >= 3) should be(Some(3))
      n(4) findSuccessor (_.edges forall (!_.isDirected)) should be(Some(2))
      n(4) isPredecessorOf n(1) shouldBe true
      validatePath(
        (n(1) pathTo n(4)).get,
        List[Outer](1, 1 ~> 3 % 5, 3, 3 ~ 4 % 1, 4)
      )
      validatePath(
        (n(1) pathUntil (_.outDegree >= 3)).get,
        List[Outer](1, 1 ~> 3 % 5, 3)
      )
      val spO = n(3) shortestPathTo n(1)
      val sp  = spO.get
      validatePath(sp, List[Outer](3, 3 ~ 4 % 1, 4, 4 ~> 5 % 0, 5, 1 ~ 5 % 3, 1))
      sp.nodes.toList should be(List(3, 4, 5, 1))
      sp.weight shouldBe 4

      def negWeight(e: g.EdgeT): Double = 5.5f - e.weight
      val spNO                          = n(3) shortestPathTo (n(1), negWeight)
      val spN                           = spNO.get
      validatePath(sp, List[Outer](3, 2 ~ 3 % 2, 2, 1 ~> 2 % 4, 1))
      spN.nodes.toList shouldBe List(3, 2, 1)
      spN.weight shouldBe 6

      val pO1 = n(4).withSubgraph(nodes = _ < 4) pathTo n(2)
      validatePath(pO1.get, List[Outer](4, 3 ~ 4 % 1, 3, 2 ~ 3 % 2, 2))
      pO1.map(_.nodes).get.toList shouldBe List(4, 3, 2)

      val pO2 = n(4).withSubgraph(edges = _.weight != 2) pathTo n(2)
      validatePath(pO2.get, List[Outer](4, 4 ~> 5 % 0, 5, 1 ~ 5 % 3, 1, 1 ~ 2 % 4, 2))
      pO2.map(_.nodes).get.toList shouldBe List(4, 5, 1, 2)
    }

    def `cycle detection`: Unit = {
      val g   = Graph(1 ~> 2, 1 ~> 3, 2 ~> 3, 3 ~> 4, 4 ~> 2)
      val fc1 = g.findCycle
      fc1.get.sameElements(List(2, 2 ~> 3, 3, 3 ~> 4, 4, 4 ~> 2, 2)) shouldBe true
      val fc2 = (g get 4).findCycle
      fc2.get.sameElements(List(4, 4 ~> 2, 2, 2 ~> 3, 3, 3 ~> 4, 4)) shouldBe true
      for {
        c1 <- fc1
        c2 <- fc2
      } yield c1 == c2 shouldBe false
      for {
        c1 <- fc1
        c2 <- fc2
      } yield c1 sameAs c2 shouldBe true
    }

    def `ordered traversal`: Unit = {
      val root = 1
      val g    = Graph(root ~> 4 % 2, root ~> 2 % 5, root ~> 3 % 4, 3 ~> 6 % 4, 3 ~> 5 % 5, 3 ~> 7 % 2)

      def edgeOrdering = g.EdgeOrdering(g.BaseInnerEdge.WeightOrdering.reverse.compare _)
      val traverser    = (g get root).outerNodeTraverser.withOrdering(edgeOrdering)

      traverser.toList should equal(List(1 to 7: _*))
    }

    def `traversers with fluent properties`: Unit = {
      val g  = Graph(1 ~> 2 % 1, 1 ~> 3 % 2, 2 ~> 3 % 3, 3 ~> 4 % 1)
      val n1 = g get 1

      n1.outerNodeTraverser.sum shouldBe 10
      g.outerNodeTraverser(n1).sum shouldBe 10
      n1.outerNodeTraverser.withMaxDepth(1).sum shouldBe 6

      n1.innerEdgeTraverser.map(_.weight).sum shouldBe 7

      n1.innerElemTraverser
        .filter {
          case g.InnerNode(n, _) => n.degree > 1
          case g.InnerEdge(_, e) => e.weight > 1
        }
        .map {
          case g.InnerNode(_, n) => n
          case g.InnerEdge(_, e) => e
        }
        .toSet shouldBe Set(1, 2, 3, 1 ~> 3 % 2, 2 ~> 3 % 3)
    }

    def `DownUp traverser`: Unit = {
      import scala.collection.mutable.ArrayBuffer

      val root      = "A"
      val g         = Graph(root ~> "B1", root ~> "B2")
      val innerRoot = g get root
      val result = innerRoot.innerNodeDownUpTraverser.foldLeft(ArrayBuffer.empty[String]) { (buf, param) =>
        param match {
          case (down, node) =>
            if (down) buf += (if (node eq innerRoot) "(" else "[") += node.toString
            else buf += (if (node eq innerRoot) ")" else "]")
        }
      }
      result.fold("")(_ + _) should (be("(A[B1][B2])") or
        be("(A[B2][B1])"))
    }

    def `extended traverser`: Unit = {
      val g = Graph(1 ~> 2, 1 ~> 3, 2 ~> 3, 3 ~> 4, 4 ~> 2)

      import g.ExtendedNodeVisitor

      type ValDepth = (Int, Int)
      var info = List.empty[ValDepth]
      (g get 1).innerNodeTraverser.foreach {
        ExtendedNodeVisitor((node, _, depth, _) => info :+= (node.outer, depth))
      }
      info.sortWith((a: ValDepth, b: ValDepth) =>
        a._1 < b._1 ||
          a._1 == b._1 && a._2 < b._2
      ) should be(List((1, 0), (2, 1), (3, 1), (4, 2)))
    }

    def `cycle detection for side effect`: Unit = {
      val g = Graph(1 ~> 2, 1 ~> 3, 2 ~> 3, 3 ~> 4, 4 ~> 2)

      var center: Option[g.NodeT] = None
      val maybeCycle = (g get 4).findCycle(n =>
        center = center match {
          case s @ Some(c) => if (n.degree > c.degree) Some(n) else s
          case None        => Some(n)
        }
      )
      maybeCycle.get.sameElements(List(2, 2 ~> 3, 3, 3 ~> 4, 4, 4 ~> 2, 2)) shouldBe true
      center.get shouldBe 2
    }

    def `weak component traverser`: Unit = {
      val componentEdges = {
        def edges(i: Int) = List(i ~> (i + 1), i ~> (i + 2), (i + 1) ~> (i + 2))
        (edges(1), edges(5))
      }
      val disconnected = Graph.from(edges = componentEdges._1 ++ componentEdges._2)
      val sums =
        for (component <- disconnected.componentTraverser())
          yield component.nodes.foldLeft(0)((cum, n) => cum + n.outer)
      sums should be(List(6, 18))

      val anyNode = disconnected.nodes.draw(new util.Random)
      anyNode.weakComponent.nodes should have size componentEdges._1.size
    }

    def `strong component traverser`: Unit = {
      type G = Graph[Char, DiEdge[Char]]
      val sccExpected: (G, G) = (
        Graph('a' ~> 'b', 'b' ~> 'c', 'c' ~> 'd', 'd' ~> 'a', 'd' ~> 'e', 'c' ~> 'e', 'e' ~> 'c'),
        Graph('f' ~> 'g', 'g' ~> 'f', 'g' ~> 'h', 'h' ~> 'j', 'j' ~> 'i', 'i' ~> 'g', 'i' ~> 'f', 'f' ~> 'i')
      )
      val connected = (sccExpected._1 union sccExpected._2) concat List('e' ~> 'f')
      val scc       = connected.strongComponentTraverser().map(_.to(Graph))
      scc.toSet should be(Set(sccExpected._1, sccExpected._2))

      val startAt = sccExpected._2.nodes.head
      startAt.strongComponents should have size 1
      startAt.innerNodeTraverser.strongComponents(_ => ())
    }

    def `path builder`: Unit = {
      val builder = g.newPathBuilder(n(1))
      builder += n(3) += n(4)
      builder.result().toOuterElems.toList shouldBe List[Outer](1, 1 ~> 3 % 5.0, 3, 3 ~ 4 % 1.0, 4)

      builder.clear()
      builder += n(4) += n(3)
      builder.result().toOuterElems.toList shouldBe List[Outer](1, 1 ~> 3 % 5.0, 3)

      builder.clear()
      builder add n(4) shouldBe false
    }
  }
}
