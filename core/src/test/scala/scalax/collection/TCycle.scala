package scalax.collection

import language.{higherKinds, postfixOps}

import GraphPredef._
import GraphEdge._
import generic.GraphCoreCompanion
import edge._
import org.scalatest.refspec.RefSpec
import org.scalatest.{Matchers, Suites}
import org.scalatest.matchers.{MatchResult, Matcher}

import scalax.collection.visualization.Visualizer

class TCycleRootTest
    extends Suites(new TCycle[immutable.Graph](immutable.Graph), new TCycle[mutable.Graph](mutable.Graph))

trait CycleMatcher[N, E[+X] <: EdgeLikeIn[X]] {
  protected type C = Graph[N, E]#Cycle

  def haveOneNodeSequenceOf(expected: Seq[N]*): Matcher[Option[C]] =
    Matcher { ns: Option[C] =>
      val found: Seq[N] = ns match {
        case None       => Seq()
        case Some(path) => path.nodes.toSeq.map(_.value).toList
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

class TCycle[CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers
    with Visualizer[CC] {

  object `given some directed graphs` extends CycleMatcher[Int, DiEdge] {

    private val acyclic_1 = factory(1 ~> 2, 1 ~> 3, 2 ~> 3, 3 ~> 4)
    private val acyclic_2 = factory(1 ~> 2, 1 ~> 3, 1 ~> 4, 1 ~> 5, 2 ~> 3, 3 ~> 7, 7 ~> 4, 7 ~> 8, 4 ~> 5, 5 ~> 6)
    private val (
      (cyclic_1, cyclicEdge_1),
      (cyclic_21, cyclicEdge_21),
      (cyclic_22, cyclicEdge_22)
    ) = {
      def makeCyclic(acyclic: CC[Int, DiEdge], byEdge: DiEdge[Int]) = {
        val cyclic = acyclic + byEdge
        (cyclic, cyclic get byEdge)
      }
      (
        makeCyclic(acyclic_1, 4 ~> 2),
        makeCyclic(acyclic_2, 8 ~> 3),
        makeCyclic(acyclic_2, 6 ~> 1)
      )
    }

    def `the cycle returned by 'findCycle' contains the expected nodes` {
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
          Seq(1, 2, 3, 7, 4, 5, 6, 1))
        n(4).findCycle should haveOneNodeSequenceOf(
          Seq(5, 6, 1, 5),
          Seq(4, 5, 6, 1, 4),
          Seq(4, 5, 6, 1, 3, 7, 4),
          Seq(4, 5, 6, 1, 2, 3, 7, 4))
      }

      val g = {
        var i, j = 0
        factory.fill(5) { i += 1; j = i + 1; i ~> j }
      }
      def fromEachNode[N, E[+X] <: EdgeLikeIn[X]](noCycles: Set[N], cycle: Graph[N, E]#Cycle) {
        given(cycle.nodes.head.containingGraph.asInstanceOf[CC[N, E]]) {
          _.nodes foreach { n =>
            val found = n.findCycle
            if (noCycles contains n.value) found should be(None)
            else (found.get sameAs cycle) should be(true)
          }
        }
      }
      given(g + 4 ~> 2) { g =>
        val cycle = g get 3 findCycle

        cycle should haveOneNodeSequenceOf(Seq(3, 4, 2, 3))
        fromEachNode(Set(5, 6), cycle get)
      }
      given(g + 5 ~> 2) { g =>
        val cycle = g get 3 findCycle

        cycle should haveOneNodeSequenceOf(Seq(3, 4, 5, 2, 3))
        fromEachNode(Set(6), cycle get)
      }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected nodes` {
      given(acyclic_1) { g =>
        g.findCycleContaining(g get 1) should be(None)
      }
      given(cyclic_1) { g =>
        g.findCycleContaining(g get 2) should haveOneNodeSequenceOf(Seq(2, 3, 4, 2))
      }
      given(acyclic_2) { g =>
        g.findCycleContaining(g get 1) should be(None)
      }
      given(cyclic_21) { g =>
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(1)) should be(None)
        g.findCycleContaining(n(3)) should haveOneNodeSequenceOf(Seq(3, 7, 8, 3))
      }
      given(cyclic_22) { g =>
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(1)) should haveOneNodeSequenceOf(
          Seq(1, 5, 6, 1),
          Seq(1, 4, 5, 6, 1),
          Seq(1, 3, 7, 4, 5, 6, 1),
          Seq(1, 2, 3, 7, 4, 5, 6, 1))
        g.findCycleContaining(n(4)) should haveOneNodeSequenceOf(
          Seq(4, 5, 6, 1, 4),
          Seq(4, 5, 6, 1, 3, 7, 4),
          Seq(4, 5, 6, 1, 2, 3, 7, 4))
        g.findCycleContaining(n(3)) should haveOneNodeSequenceOf(Seq(3, 7, 4, 5, 6, 1, 3), Seq(3, 7, 4, 5, 6, 1, 2, 3))
        g.findCycleContaining(n(2)) should haveOneNodeSequenceOf(Seq(2, 3, 7, 4, 5, 6, 1, 2))
      }
    }
    /* TODO withSubgraph node predicate type mismatch
    def `the cycle returned by 'partOfCycle' combined with fluent properties contains the expected nodes` {
      given(cyclic_22) { g =>
        def n(outer: Int) = g get outer

        n(1).withSubgraph(nodes = _ != 3) partOfCycle () should haveOneNodeSequenceOf(
          Seq(1, 5, 6, 1),
          Seq(1, 4, 5, 6, 1))
        n(4).withSubgraph(nodes = _ != 3) partOfCycle () should haveOneNodeSequenceOf(Seq(4, 5, 6, 1, 4))
        n(2).withSubgraph(nodes = _ != 3) partOfCycle () should be(None)
      }
    }
    */
    def `the cycle returned by 'findCycle' contains the expected edges` {
      given(acyclic_1) { _.findCycle should be(None) }
      given(cyclic_1)  { _.findCycle.get.edges should contain(cyclicEdge_1) }
      given(acyclic_2) { _.findCycle should be(None) }
      given(cyclic_21) { _.findCycle.get.edges should contain(cyclicEdge_21) }
      given(cyclic_22) { _.findCycle.get.edges should contain(cyclicEdge_22) }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected edges` {
      given(cyclic_1) { g =>
        g.findCycleContaining(g get 2).get.edges should contain(cyclicEdge_1)
      }
      given(cyclic_21) { g =>
        g.findCycleContaining(g get 3).get.edges should contain(cyclicEdge_21)
      }
      given(cyclic_22) { g =>
        g.findCycleContaining(g get 1).get.edges should contain(cyclicEdge_22)
      }
    }

    def `'isCyclic' returns the expected result` {
      given(acyclic_1) { _ should be('isAcyclic) }
      given(cyclic_1) { _ should be('isCyclic) }
      given(acyclic_2) { _ should be('isAcyclic) }
      given(cyclic_21) { _ should be('isCyclic) }
      given(cyclic_22) { _ should be('isCyclic) }
    }

    def `they are cyclic if they contain a self loop #76` {
      val loop = 1 ~> 1
      given(acyclic_1 + loop) { _ should be('isCyclic) }
      given(factory(loop)) { g =>
        g.findCycle should (be('isDefined) and beValid)
        g.findCycleContaining(g get 1) should (be('isDefined) and beValid)
      }
    }
  }

  object `given some undirected graphs` extends CycleMatcher[Int, UnDiEdge] {

    private val unDiAcyclic_1 = factory(1 ~ 2, 2 ~ 3)
    private val unDiCyclic_1  = unDiAcyclic_1 + 1 ~ 3

    private val unDiAcyclic_2 = factory(1 ~ 2, 1 ~ 3, 2 ~ 4, 2 ~ 5)
    private val unDiCyclic_21 = unDiAcyclic_2 + 3 ~ 5
    private val unDiCyclic_22 = unDiAcyclic_2 ++ List(3 ~ 6, 6 ~ 7, 7 ~ 4)

    def `the cycle returned by 'findCycle' contains the expected nodes` {
      given(unDiAcyclic_1) { g =>
        (g get 1 findCycle) should be(None)
      }
      given(unDiCyclic_1) { g =>
        (g get 2 findCycle) should haveOneNodeSequenceOf(Seq(2, 3, 1, 2), Seq(2, 1, 3, 2))
      }
      given(unDiAcyclic_2) { g =>
        (g get 1 findCycle) should be(None)
      }
      given(unDiCyclic_21) { g =>
        (g get 1 findCycle) should haveOneNodeSequenceOf(Seq(1, 3, 5, 2, 1), Seq(1, 2, 5, 3, 1))
      }
      given(unDiCyclic_22) { g =>
        (g get 3 findCycle) should haveOneNodeSequenceOf(Seq(3, 1, 2, 4, 7, 6, 3), Seq(3, 6, 7, 4, 2, 1, 3))
      }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected nodes` {
      given(unDiAcyclic_1) { g =>
        g.findCycleContaining(g get 1) should be(None)
      }
      given(unDiCyclic_1) { g =>
        g.findCycleContaining(g get 2) should haveOneNodeSequenceOf(Seq(2, 3, 1, 2), Seq(2, 1, 3, 2))
      }
      given(unDiAcyclic_2) { g =>
        g.findCycleContaining(g get 1) should be(None)
      }
      given(unDiCyclic_21) { g =>
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(1)) should haveOneNodeSequenceOf(Seq(1, 3, 5, 2, 1), Seq(1, 2, 5, 3, 1))
        g.findCycleContaining(n(4)) should be(None)
      }
      given(unDiCyclic_22) { g =>
        def n(outer: Int) = g get outer

        g.findCycleContaining(n(3)) should haveOneNodeSequenceOf(Seq(3, 1, 2, 4, 7, 6, 3), Seq(3, 6, 7, 4, 2, 1, 3))
        g.findCycleContaining(n(5)) should be(None)
      }
    }
    /* TODO withSubgraph node predicate type mismatch
    def `the cycle returned by 'partOfCycle' combined with fluent properties contains the expected nodes` {
      given(unDiCyclic_21) { g =>
        (g get 1).withSubgraph(nodes = _ != 2) partOfCycle () should be(None)
      }
      given(unDiCyclic_22) { g =>
        (g get 3).withSubgraph(nodes = _ != 2) partOfCycle () should be(None)
      }
    }
    */
  }

  object `given an undirected multigraph` {

    private val (e1, e2) = (WkUnDiEdge(1, 2)(0), WkUnDiEdge(1, 2)(1))
    private val g        = factory(e1, e2)

    def `the cycle returned by 'findCycle' contains the expected edges` {
      given(g) { g =>
        val c = (g get 1).findCycle
        c should be('isDefined)
        c.get.edges should (be(List(e1, e2)) or
          be(List(e2, e1)))
      }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected edges` {
      given(g) { g =>
        val c = g.findCycleContaining(g get 1)
        c should be('isDefined)
        c.get.edges should (be(List(e1, e2)) or
          be(List(e2, e1)))
      }
    }
  }

  object `given some mixed graphs` extends CycleMatcher[Int, UnDiEdge] {

    private val mixed = factory(Data.elementsOfUnDi_1: _*)

    def `'findCycle' finds a cycle following any route` {
      given(
        factory(
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
        )) { g =>
        g.findCycle should (be('isDefined) and beValid)
        (g get 13).innerNodeTraverser.withOrdering(g.NodeOrdering((a, b) => b.value - a.value)).findCycle should (be(
          'isDefined) and beValid)
      }
      given(mixed - 5 - 4 ~> 4) { g =>
        (g get 1).findCycle should haveOneNodeSequenceOf(Seq(1, 3, 2, 1))
      }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected nodes` {
      given(mixed) { g =>
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
          Seq(1, 3, 4, 5, 1))
        g.findCycleContaining(n(4)) should haveOneNodeSequenceOf(
          Seq(4, 4),
          Seq(4, 5, 3, 4),
          Seq(4, 5, 1, 2, 3, 4),
          Seq(4, 5, 1, 3, 4))
      }
    }

    def `the cycle returned by 'partOfCycle' combined with fluent properties contains the expected nodes` {
      given(mixed) { g =>
        def n(outer: Int) = g get outer

        n(2).withSubgraph(edges = _ != DiEdge(1, 3)).partOfCycle should haveOneNodeSequenceOf(
          Seq(2, 3, 4, 5, 1, 2),
          Seq(2, 1, 5, 3, 2),
          Seq(2, 3, 5, 1, 2))
        n(2)
          .withSubgraph(nodes = _ != 5)
          .withOrdering(g.EdgeOrdering(g.Edge.WeightOrdering.compare))
          .partOfCycle should haveOneNodeSequenceOf(Seq(2, 1, 3, 2))

        n(1).withSubgraph(nodes = _ != 5).partOfCycle should haveOneNodeSequenceOf(Seq(1, 3, 2, 1))
      }
    }

    private val g          = factory(1 ~ 2, 1 ~> 2, 2 ~ 3)
    private val cycleEdges = List(1 ~> 2, 1 ~ 2)

    def `the cycle returned by 'findCycle' contains the expected edges` {
      given(g) { g =>
        g.graphSize should be(3)
        g.nodes foreach { n =>
          val c = n.findCycle
          (n, c.isDefined) should be((n, true))
          c.get.edges should (be(cycleEdges) or be(cycleEdges.reverse))
        }
      }
    }

    def `the cycle returned by 'findCycleContaining' contains the expected edges` {
      given(g) { g =>
        g.nodes.filterNot(_.toOuter == 3) foreach { n =>
          val c = g.findCycleContaining(g get n)
          (n, c.isDefined) should be((n, true))
          c.get.edges should (be(cycleEdges) or
            be(cycleEdges.reverse))
        }
      }
    }
  }
}
