package scalax.collection

import language.postfixOps

import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.visualization.Visualizer

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.Suites
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.chaining.scalaUtilChainingOps

class CycleSpec extends Suites(new Cycle[immutable.Graph](immutable.Graph), new Cycle[mutable.Graph](mutable.Graph))

trait CycleMatcher[N, E <: Edge[N]] {
  protected type C = Graph[N, E]#Cycle

  def haveOneNodeSequenceOf(expected: Seq[N]*): Matcher[Option[C]] =
    Matcher { ns: Option[C] =>
      val found: Seq[N] = ns match {
        case None       => Seq()
        case Some(path) => path.nodes.toSeq.map(_.outer).toList
      }
      def msg(key: String): String =
        s"""$found equals to $key of
           |${expected mkString ", "} in
           |${ns.get.containingGraph}.
         """.stripMargin
      MatchResult(expected contains found, msg("none"), msg("one"))
    }

  def beValid: Matcher[Option[C]] =
    Matcher { c: Option[C] =>
      def msg(key: String): String = s"$c $key valid"
      MatchResult(c.get.isValid, msg("is not"), msg("is"))
    }
}

class Cycle[CC[N, E <: Edge[N]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers
    with Visualizer[CC] {

  object `given some directed graphs` extends CycleMatcher[Int, DiEdge[Int]] {

    private val acyclic_1 = factory(1 ~> 2, 1 ~> 3, 2 ~> 3, 3 ~> 4)
    private val acyclic_2 = factory(1 ~> 2, 1 ~> 3, 1 ~> 4, 1 ~> 5, 2 ~> 3, 3 ~> 7, 7 ~> 4, 7 ~> 8, 4 ~> 5, 5 ~> 6)
    private val (
      (cyclic_1, cyclicEdge_1),
      (cyclic_21, cyclicEdge_21),
      (cyclic_22, cyclicEdge_22)
    ) = {
      def makeCyclic(acyclic: CC[Int, DiEdge[Int]], byEdge: DiEdge[Int]) = {
        val cyclic = acyclic concat List(byEdge)
        (cyclic, cyclic get byEdge)
      }
      (
        makeCyclic(acyclic_1, 4 ~> 2),
        makeCyclic(acyclic_2, 8 ~> 3),
        makeCyclic(acyclic_2, 6 ~> 1)
      )
    }

    def `the cycle returned by 'findCycle' contains the expected nodes`: Unit = {
      given(acyclic_1) { g =>
        (g get 1 findCycle) should be(None)
      }
      given(cyclic_1) { g =>
        (g get 2 findCycle) should haveOneNodeSequenceOf(Seq(2, 3, 4, 2))
      }

      given(acyclic_2) { g =>
        (g get 1 findCycle) should be(None)
      }
      given(cyclic_21) { g =>
        (g get 1 findCycle) should haveOneNodeSequenceOf(Seq(3, 7, 8, 3))
      }
      given(cyclic_22) { g =>
        def n(outer: Int) = g get outer

        n(1).findCycle should haveOneNodeSequenceOf(
          Seq(1, 5, 6, 1),
          Seq(1, 4, 5, 6, 1),
          Seq(1, 3, 7, 4, 5, 6, 1),
          Seq(1, 2, 3, 7, 4, 5, 6, 1)
        )
        n(4).findCycle should haveOneNodeSequenceOf(
          Seq(5, 6, 1, 5),
          Seq(4, 5, 6, 1, 4),
          Seq(4, 5, 6, 1, 3, 7, 4),
          Seq(4, 5, 6, 1, 2, 3, 7, 4)
        )
      }

      val g = {
        var i, j = 0
        factory.fill(5) { i += 1; j = i + 1; i ~> j }
      }
      def fromEachNode[N, E <: Edge[N]](noCycles: Set[N], cycle: Graph[N, E]#Cycle): Unit =
        given(cycle.nodes.head.containingGraph.asInstanceOf[CC[N, E]]) {
          case g: Graph[N, E] => // `annotated for IntelliJ
            g.nodes foreach { n =>
              val found = n.findCycle
              if (noCycles contains n.outer) found should be(None)
              else (found.get sameAs cycle) should be(true)
            }
        }
      given(g concat List(4 ~> 2)) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        val cycle = g get 3 findCycle

        cycle should haveOneNodeSequenceOf(Seq(3, 4, 2, 3))
        fromEachNode(Set(5, 6), cycle get)
      }
      given(g concat List(5 ~> 2)) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        val cycle = g get 3 findCycle

        cycle should haveOneNodeSequenceOf(Seq(3, 4, 5, 2, 3))
        fromEachNode(Set(6), cycle get)
      }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected nodes`: Unit = {
      given(acyclic_1) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 1) should be(None)
      }
      given(cyclic_1) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 2) should haveOneNodeSequenceOf(Seq(2, 3, 4, 2))
      }
      given(acyclic_2) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 1) should be(None)
      }
      given(cyclic_21) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(1)) should be(None)
        g.findCycleContaining(n(3)) should haveOneNodeSequenceOf(Seq(3, 7, 8, 3))
      }
      given(cyclic_22) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(1)) should haveOneNodeSequenceOf(
          Seq(1, 5, 6, 1),
          Seq(1, 4, 5, 6, 1),
          Seq(1, 3, 7, 4, 5, 6, 1),
          Seq(1, 2, 3, 7, 4, 5, 6, 1)
        )
        g.findCycleContaining(n(4)) should haveOneNodeSequenceOf(
          Seq(4, 5, 6, 1, 4),
          Seq(4, 5, 6, 1, 3, 7, 4),
          Seq(4, 5, 6, 1, 2, 3, 7, 4)
        )
        g.findCycleContaining(n(3)) should haveOneNodeSequenceOf(Seq(3, 7, 4, 5, 6, 1, 3), Seq(3, 7, 4, 5, 6, 1, 2, 3))
        g.findCycleContaining(n(2)) should haveOneNodeSequenceOf(Seq(2, 3, 7, 4, 5, 6, 1, 2))
      }
    }

    def `the cycle returned by 'partOfCycle' combined with fluent properties contains the expected nodes`: Unit =
      given(cyclic_22) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        def n(outer: Int) = g get outer

        n(1).withSubgraph(nodes = _ != 3).partOfCycle() should haveOneNodeSequenceOf(
          Seq(1, 5, 6, 1),
          Seq(1, 4, 5, 6, 1)
        )
        n(4).withSubgraph(nodes = _ != 3).partOfCycle() should haveOneNodeSequenceOf(Seq(4, 5, 6, 1, 4))
        n(2).withSubgraph(nodes = _ != 3).partOfCycle() should be(None)
      }

    def `the cycle returned by 'findCycle' contains the expected edges`: Unit = {
      given(acyclic_1)(_.findCycle should be(None))
      given(cyclic_1)(_.findCycle.get.edges should contain(cyclicEdge_1))
      given(acyclic_2)(_.findCycle should be(None))
      given(cyclic_21)(_.findCycle.get.edges should contain(cyclicEdge_21))
      given(cyclic_22)(_.findCycle.get.edges should contain(cyclicEdge_22))
    }

    def `the cycle returned by 'findCycleContaining' contains the expected edges`: Unit = {
      given(cyclic_1) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 2).get.edges should contain(cyclicEdge_1)
      }
      given(cyclic_21) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 3).get.edges should contain(cyclicEdge_21)
      }
      given(cyclic_22) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 1).get.edges should contain(cyclicEdge_22)
      }
    }

    def `'isCyclic' returns the expected result`: Unit = {
      given(acyclic_1)(_.isAcyclic shouldBe true)
      given(cyclic_1)(_.isCyclic shouldBe true)
      given(acyclic_2)(_.isAcyclic shouldBe true)
      given(cyclic_21)(_.isCyclic shouldBe true)
      given(cyclic_22)(_.isCyclic shouldBe true)
    }

    def `they are cyclic if they contain a self loop #76`: Unit = {
      val loop = 1 ~> 1
      given(acyclic_1 concat List(loop))(_.isCyclic shouldBe true)
      given(factory(loop)) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        g.findCycle should (be(defined) and beValid)
        g.findCycleContaining(g get 1) should (be(defined) and beValid)
      }
    }
  }

  object `given some undirected graphs` extends CycleMatcher[Int, UnDiEdge[Int]] {

    private val unDiAcyclic_1 = factory(1 ~ 2, 2 ~ 3)
    private val unDiCyclic_1  = unDiAcyclic_1 concat List(1 ~ 3)

    private val unDiAcyclic_2 = factory(1 ~ 2, 1 ~ 3, 2 ~ 4, 2 ~ 5)
    private val unDiCyclic_21 = unDiAcyclic_2 concat List(3 ~ 5)
    private val unDiCyclic_22 = unDiAcyclic_2 concat List(3 ~ 6, 6 ~ 7, 7 ~ 4)

    def `the cycle returned by 'findCycle' contains the expected nodes`: Unit = {
      given(unDiAcyclic_1) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        (g get 1 findCycle) shouldBe None
      }
      given(unDiCyclic_1) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        (g get 2 findCycle) should haveOneNodeSequenceOf(Seq(2, 3, 1, 2), Seq(2, 1, 3, 2))
      }
      given(unDiAcyclic_2) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        (g get 1 findCycle) should be(None)
      }
      given(unDiCyclic_21) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        (g get 1 findCycle) should haveOneNodeSequenceOf(Seq(1, 3, 5, 2, 1), Seq(1, 2, 5, 3, 1))
      }
      given(unDiCyclic_22) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        (g get 3 findCycle) should haveOneNodeSequenceOf(Seq(3, 1, 2, 4, 7, 6, 3), Seq(3, 6, 7, 4, 2, 1, 3))
      }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected nodes`: Unit = {
      given(unDiAcyclic_1) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 1) should be(None)
      }
      given(unDiCyclic_1) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 2) should haveOneNodeSequenceOf(Seq(2, 3, 1, 2), Seq(2, 1, 3, 2))
      }
      given(unDiAcyclic_2) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        g.findCycleContaining(g get 1) should be(None)
      }
      given(unDiCyclic_21) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(1)) should haveOneNodeSequenceOf(Seq(1, 3, 5, 2, 1), Seq(1, 2, 5, 3, 1))
        g.findCycleContaining(n(4)) should be(None)
      }
      given(unDiCyclic_22) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(3)) should haveOneNodeSequenceOf(Seq(3, 1, 2, 4, 7, 6, 3), Seq(3, 6, 7, 4, 2, 1, 3))
        g.findCycleContaining(n(5)) should be(None)
      }
    }
    def `the cycle returned by 'partOfCycle' combined with fluent properties contains the expected nodes`: Unit = {
      given(unDiCyclic_21) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        (g get 1).withSubgraph(nodes = _ != 2).partOfCycle() should be(None)
      }
      given(unDiCyclic_22) { case g: Graph[Int, UnDiEdge[Int]] => // `annotated for IntelliJ
        (g get 3).withSubgraph(nodes = _ != 2).partOfCycle() should be(None)
      }
    }
  }

  /* TODO L
  object `given an undirected multigraph` {

    private val (e1, e2) = (WkUnDiEdge(1, 2)(0), WkUnDiEdge(1, 2)(1))
    private val g        = factory(e1, e2)

    def `the cycle returned by 'findCycle' contains the expected edges`: Unit =
      given(g) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        val c = (g get 1).findCycle
        c shouldBe defined
        c.get.edges should (be(List(e1, e2)) or be(List(e2, e1)))
      }

    def `the cycle returned by 'findCycleContaining' contains the expected edges`: Unit =
      given(g) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
        val c = g.findCycleContaining(g get 1)
        c shouldBe defined
        c.get.edges should (be(List(e1, e2)) or
          be(List(e2, e1)))
      }
  }
   */

  object `given some mixed graphs` extends CycleMatcher[Int, AnyEdge[Int]] {

    private val mixed = factory.from(Data.elementsOfMixed_1)

    def `'findCycle' finds a cycle following any route`: Unit = {
      given(
        factory[Int, AnyEdge](
          1 ~> 3,
          3 ~> 4,
          3 ~> 20,
          20 ~> 21,
          1 ~> 10,
          10 ~ 11,
          11 ~ 12,
          12 ~ 13,
          12 ~> 3,
          20 ~> 10
        )
      ) { case g: Graph[Int, AnyEdge[Int]] => // `annotated for IntelliJ
        g.findCycle pipe { cycle =>
          cycle should (be(defined) and beValid)
          cycle foreach (_.nodes foreach { n =>
            g.innerNodeTraverser(n).findCycle should (be(defined) and beValid)
          })
        }
        (g get 13).innerNodeTraverser.withOrdering(g.NodeOrdering((a, b) => b.outer - a.outer)).findCycle should (be(
          defined
        ) and beValid)
      }
      given(mixed.filterNot(_ == 5, _ == 4 ~> 4)) { case g: Graph[Int, AnyEdge[Int]] => // `annotated for IntelliJ
        (g get 1).findCycle should haveOneNodeSequenceOf(Seq(1, 3, 2, 1))
      }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected nodes`: Unit =
      given(mixed) { case g: Graph[Int, AnyEdge[Int]] => // `annotated for IntelliJ
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(2)) should haveOneNodeSequenceOf(
          Seq(2, 1, 3, 2),
          Seq(2, 3, 4, 5, 1, 2),
          Seq(2, 1, 5, 3, 2),
          Seq(2, 3, 5, 1, 2)
        )
        g.findCycleContaining(n(1)) should haveOneNodeSequenceOf(
          Seq(1, 3, 2, 1),
          Seq(1, 3, 5, 1),
          Seq(1, 2, 3, 5, 1),
          Seq(1, 5, 3, 2, 1),
          Seq(1, 2, 3, 4, 5, 1),
          Seq(1, 3, 4, 5, 1)
        )
        g.findCycleContaining(n(4)) should haveOneNodeSequenceOf(
          Seq(4, 4),
          Seq(4, 5, 3, 4),
          Seq(4, 5, 1, 2, 3, 4),
          Seq(4, 5, 1, 3, 4)
        )
      }

    def `the cycle returned by 'partOfCycle' combined with fluent properties contains the expected nodes`: Unit =
      given(mixed) { case g: Graph[Int, AnyEdge[Int]] => // `annotated for IntelliJ
        def n(outer: Int) = g get outer

        n(2).withSubgraph(edges = _ != DiEdge(1, 3)).partOfCycle should haveOneNodeSequenceOf(
          Seq(2, 3, 4, 5, 1, 2),
          Seq(2, 1, 5, 3, 2),
          Seq(2, 3, 5, 1, 2)
        )
        n(2)
          .withSubgraph(nodes = _ != 5)
          .withOrdering(g.BaseInnerEdge.WeightOrdering)
          .partOfCycle should haveOneNodeSequenceOf(Seq(2, 1, 3, 2))

        n(1).withSubgraph(nodes = _ != 5).partOfCycle should haveOneNodeSequenceOf(Seq(1, 3, 2, 1))
      }

    private val cycleEdges = List(1 ~> 2, 1 ~ 2)
    private val g          = factory.from[Int, AnyEdge](2 ~ 3 +: cycleEdges)

    def `the cycle returned by 'findCycle' contains the expected edges`: Unit =
      given(g) { case g: Graph[Int, AnyEdge[Int]] => // `annotated for IntelliJ
        g.size should be(3)
        g.nodes foreach { n =>
          val c = n.findCycle
          (n, c.isDefined) should be((n, true))
          c.get.edges should (be(cycleEdges) or be(cycleEdges.reverse))
        }
      }

    def `the cycle returned by 'findCycleContaining' contains the expected edges`: Unit =
      given(g) { case g: Graph[Int, AnyEdge[Int]] => // `annotated for IntelliJ
        g.nodes.filterNot(_.outer == 3) foreach { n =>
          val c = g.findCycleContaining(g get n)
          (n, c.isDefined) should be((n, true))
          c.get.edges should (be(cycleEdges) or be(cycleEdges.reverse))
        }
      }
  }
}
