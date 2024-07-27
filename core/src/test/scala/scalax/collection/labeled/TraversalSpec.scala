package scalax.collection
package labeled

import scala.concurrent.duration._

//import scala.util.Random
//import org.scalacheck.Arbitrary.arbitrary
//import org.scalacheck._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import Data._
import OuterImplicits._
//import generator.GraphGen
import generic.{AnyEdge, Edge, GenericGraphCoreFactory}
import edges._
import edges.labeled._
import edges.multilabeled._
import GraphTraversal._
import visualization.Visualizer

import scala.collection.mutable.ListBuffer

class TraversalSpec
    extends Suites(
      new Traversal[immutable.Graph](immutable.Graph),
      new Traversal[mutable.Graph](mutable.Graph)
    )

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

  def `assert bug 9 of shortestPathTo is fixed`: Unit =
    withGraph(factory(0 ~> 1 % 3, 0 ~> 2 % 4, 1 ~> 3 % 3, 2 ~> 3 % 1)) { g =>
      def n(outer: Int) = g get outer
      (n(0) shortestPathTo n(3)).get.nodes.toList should be(List(0, 2, 3))
    }

  def `shortestPathTo in WDi_1`: Unit =
    withGraph(factory(elementsOfWDi_1: _*)) { g =>
      def n(outer: Int) = g get outer

      n(5) shortestPathTo n(4) shouldBe None
      n(5) shortestPathTo n(1) shouldBe None
      n(3) shortestPathTo n(1) shouldBe None

      (n(1) shortestPathTo n(3)).get.nodes.toList shouldBe List(1, 3)
      (n(4) shortestPathTo n(5)).get.nodes.toList shouldBe List(4, 3, 5)
      (n(1) shortestPathTo n(5)).get.nodes.toList shouldBe List(1, 5)
    }

  def `shortestPathTo in WDi_1 using Float`: Unit =
    withGraph(factory(elementsOfWDi_1: _*)) { g =>
      def n(outer: Int) = g get outer

      def weight(e: g.EdgeT): Float         = 0.5f + e.weight.toFloat
      def reverseWeight(e: g.EdgeT): Double = 41 - e.weight

      n(5) shortestPathTo (n(4), weight) shouldBe empty

      (n(1) shortestPathTo (n(3), weight)).get.nodes.to(LazyList) should contain theSameElementsInOrderAs Array(1, 3)
      (n(1) shortestPathTo (n(3), reverseWeight)).get.nodes.to(LazyList) should contain theSameElementsInOrderAs Array(
        1,
        2,
        3
      )
    }

  def `shortestPathTo in WUnDi_1`: Unit =
    withGraph(factory(elementsOfWMixed_1: _*)) { g =>
      def shortestPathNodes(from: Int, to: Int): LazyList[g.NodeT] = {
        def n(value: Int): g.NodeT = g get value

        val path = n(from) shortestPathTo n(to)
        path shouldBe defined
        path.get.nodes.to(LazyList)
      }
      shortestPathNodes(2, 5) should contain theSameElementsInOrderAs Array(2, 3, 4, 5)
      shortestPathNodes(4, 5) should contain theSameElementsInOrderAs Array(4, 5)
      shortestPathNodes(1, 3) should (contain theSameElementsInOrderAs (Array(1, 3)) or
        contain theSameElementsInOrderAs (Array(1, 5, 3)))
      shortestPathNodes(5, 4) should contain theSameElementsInOrderAs Array(5, 3, 4)
      shortestPathNodes(3, 1) should contain theSameElementsInOrderAs Array(3, 4, 5, 1)
    }

  def `shortestPathTo withMaxDepth`: Unit =
    withGraph(factory(elementsOfWMixed_1: _*)) { g =>
      def n(value: Int): g.NodeT = g get value

      n(2).innerNodeTraverser.withMaxDepth(2).shortestPathTo(n(5)).get.nodes.toList should be(List(2, 3, 5))
    }

  def `shortestPathTo withMaxWeight`: Unit =
    withGraph(factory(elementsOfWMixed_1: _*)) { g =>
      def n(value: Int): g.NodeT = g get value

      val t = n(2).innerNodeTraverser
      t.withMaxWeight(3).shortestPathTo(n(5)) shouldBe defined
      t.withMaxWeight(2).shortestPathTo(n(5)) shouldBe empty
    }

  // see diagram WUnDi-2.jpg
  val eUnDi_2 = List[AnyEdge[Int]](1 ~ 2 %% 4, 2 ~ 3 %% -1, 1 ~> 3 %% 5, 1 ~ 3 %% 4, 1 ~> 2 %% 3, 2 ~ 2 %% 1)
  val gUnDi_2 =
    factory.from[Int, AnyEdge[Int]](Set.empty, eUnDi_2).asAnyGraph

  def `shortestPathTo in UnDi_2`: Unit =
    withGraph(gUnDi_2) { g =>
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
      p3_3.edges.toList shouldBe empty
    }

  def `traverser withSubgraph`: Unit =
    withGraph(gUnDi_2) { g =>
      def n(value: Int) = g get value

      val p2_1_nNE3 = n(2).withSubgraph(nodes = _ != 3).pathTo(n(1)).get
      p2_1_nNE3.nodes.toList should be(List(2, 1))
      p2_1_nNE3.edges.toList should be(List(2 ~ 1 %% 4))

      val p1_3_wGT4 = n(1).withSubgraph(edges = _.weight > 4).pathTo(n(3)).get
      p1_3_wGT4.nodes.toList should be(List(1, 3))
      p1_3_wGT4.edges.toList should be(List(eUnDi_2(2)))

      val p1_3_wLT4 = n(1).withSubgraph(edges = _.weight < 4).pathTo(n(3)).get
      p1_3_wLT4.nodes.toList should be(List(1, 2, 3))
      p1_3_wLT4.edges.toList should be(List(eUnDi_2(4), eUnDi_2(1)))
    }

  object `traverser withMaxWeight` {
    object WMixed_1 extends TGraph[Int, AnyEdge[Int], G](factory.from(elementsOfWMixed_1))
    import WMixed_1._

    private def check(kind: Kind): Unit =
      List[Long](Long.MaxValue, 5, 4, 3, 2, 1, 0).map(max => n(1).withKind(kind).withMaxWeight(max).size) shouldBe
        List(5, 4, 3, 2, 1, 1, 1)

    def `calling DepthFirst`: Unit = withGraph(WMixed_1.g) { _ =>
      check(DepthFirst)
    }
    def `calling BreadthFirst`: Unit = withGraph(WMixed_1.g) { _ =>
      check(BreadthFirst)
    }
  }

  def `traverser to graph`: Unit = {
    object Di_1 extends TGraph(factory(elementsOfDi_1: _*))
    withGraph(Di_1.g) { g =>
      def innerNode(outer: Int) = g get outer

      innerNode(1).outerNodeTraverser.to(factory) should equal(factory(1 ~> 2, 2 ~> 3, 3 ~> 5, 1 ~> 5, 1 ~> 3))

      innerNode(2).outerNodeTraverser(anyConnected).to(factory) should equal(
        factory(1 ~> 2, 2 ~> 3, 4 ~> 3, 3 ~> 5, 1 ~> 5, 1 ~> 3)
      )

      innerNode(3).outerNodeTraverser(predecessors).to(factory) should equal(factory(4 ~> 3, 1 ~> 3, 2 ~> 3, 1 ~> 2))
    }
  }

  def `traverser with a visitor`: Unit =
    withGraph(gUnDi_2) { g =>
      def n(value: Int) = g get value

      val nodes     = ListBuffer[g.NodeT]()
      val edges     = ListBuffer[g.EdgeT]()
      val traverser = n(2).innerElemTraverser.withSubgraph(nodes = _ != 3)
      traverser.pathTo(n(1)) {
        case n: g.InnerNode => nodes += n.asNodeT
        case e: g.InnerEdge => edges += e.asEdgeT
      }

      nodes shouldBe List(n(2), n(1))
      edges.toList.sorted(g.BaseInnerEdge.WeightOrdering) shouldBe List(eUnDi_2(1), eUnDi_2(5), eUnDi_2(0))
    }

  def `shortestPathTo in the flight example graph`: Unit = {
    import scalax.collection.labeled.aviation._

    val (jfc, lhr, dme, svx, fra, prg) =
      (Airport("JFC"), Airport("LHR"), Airport("DME"), Airport("SVX"), Airport("FRA"), Airport("PRG"))

    val flights: List[Flight] =
      List(
        jfc ~> dme :++ ("UN 2222", Nil, 8.hours + 50.minutes),
        dme ~> svx :++ ("UN 109", Nil, 2.hours + 15.minutes),
        jfc ~> lhr :++ ("BA 174", Nil, 6.hours + 50.minutes),
        jfc ~> fra :++ ("LH 400", Nil, 8.hours + 20.minutes),
        jfc ~> fra :++ ("UA 8840", Nil, 7.hours + 35.minutes),
        lhr ~> dme :++ ("BA 872", Nil, 4.hours),
        lhr ~> dme :++ ("SU 242", Nil, 3.hours + 50.minutes),
        lhr ~> fra :++ ("LH 903", Nil, 1.hours + 35.minutes),
        lhr ~> prg :++ ("BA 860", Nil, 2.hours),
        fra ~> lhr :++ ("LH 920", Nil, 1.hours + 35.minutes),
        fra ~> dme :++ ("LH 1444", Nil, 3.hours + 10.minutes),
        fra ~> svx :++ ("LH 1480", Nil, 4.hours + 35.minutes),
        prg ~> svx :++ ("U6 902", Nil, 4.hours + 25.minutes)
      )

    def flight(flightNo: String) = flights.find(_.flightNo == flightNo).get

    val g = factory.from[Airport, Flight](Set.empty, flights)

    withGraph(g.asAnyGraph) { g =>
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
      (g get jfc).innerEdgeTraverser.shortestPathTo(g get lhr) { (e: g.EdgeT) =>
        visited += e
      }
      val visitedSorted = visited.toList.sortWith((a: g.EdgeT, b: g.EdgeT) => a.flightNo < b.flightNo)
      visitedSorted.sameElements(
        List(flight("BA 174"), flight("LH 400"), flight("UA 8840"), flight("UN 2222"))
      ) should be(true)
    }
  }

  def `traverser withOrdering for edges`: Unit = {
    val outerEdges = List(
      1 ~> 4 % 2,
      1 ~> 2 % 5,
      1 ~> 3 % 4,
      3 ~> 6 % 4,
      3 ~> 5 % 5,
      3 ~> 7 % 2
    )
    withGraph(factory(outerEdges: _*)) { g =>
      val root = g get 1

      def edgeOrdering = g EdgeOrdering (g.BaseInnerEdge.WeightOrdering.reverse.compare _)

      val orderedTraverser = root.outerNodeTraverser.withOrdering(edgeOrdering)
      orderedTraverser.toList should be(List(1 to 7: _*))
      orderedTraverser.withKind(DepthFirst).toList should be(List(1, 2, 3, 5, 6, 7, 4))
    }
  }

  /* TODO GraphGen
  def `shortest path exists if path exists`: Unit = {
    implicit val arbitraryWDiGraph = Arbitrary {
      import GraphGen.SmallInt._
      new GraphGen[Int, WDiEdge[Int], G](factory, order, nodeGen, nodeDegrees, Set(WDiEdge), connected).apply
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
   */
}
