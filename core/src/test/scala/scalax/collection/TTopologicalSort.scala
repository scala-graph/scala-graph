package scalax.collection

import GraphPredef._
import GraphEdge._
import GraphTraversal._
import GraphTraversal.Parameters._
import generic.GraphCoreCompanion
import edge.Implicits._

import org.scalatest._
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.refspec.RefSpec

import scalax.collection.edge.WkDiEdge
import scalax.collection.visualization.Visualizer

class TTopologicalSortRootTest
    extends Suites(
      new TTopologicalSort[immutable.Graph](immutable.Graph),
      new TTopologicalSort[mutable.Graph](mutable.Graph)
    )

private class TTopologicalSort[G[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]](
    val factory: GraphCoreCompanion[G]
) extends RefSpec
    with should.Matchers
    with ScalaCheckPropertyChecks
    with Visualizer[G] {

  private object Topo {

    class Checker[N, E[+X] <: EdgeLikeIn[X]](val graph: G[N, E]) {

      def checkOuterNodes(seq: Iterable[N]): Unit =
        checkInnerNodes(seq map (graph get _))

      type OrderedInnerNodes = Iterable[graph.NodeT]

      def checkInnerNodes(
          seq: OrderedInnerNodes,
          root: Option[graph.NodeT] = None,
          ignorePredecessors: Boolean = false
      ): Unit = {
        def predecessors(maybeRoot: Option[graph.NodeT]): Set[graph.NodeT] =
          maybeRoot.fold(
            ifEmpty = Set.empty[graph.NodeT]
          ) { root =>
            root.innerNodeTraverser().withParameters(Dfs(Predecessors)).toSet - root
          }

        def checkOrder(seq: OrderedInnerNodes, ignorePredecessorsOf: Option[graph.NodeT]): Unit =
          seq.foldLeft(predecessors(ignorePredecessorsOf)) { (allowedPredecessors, innerNode) =>
            if (!innerNode.diPredecessors.forall(allowedPredecessors.contains))
              fail(s"$innerNode is misplaced in $seq")
            allowedPredecessors + innerNode
          }

        def checkCompleteness(
            seq: OrderedInnerNodes,
            maybeRoot: Option[graph.NodeT],
            ignorePredecessors: Boolean
        ): Unit = {
          val expected = maybeRoot.fold(
            ifEmpty = graph.nodes.toSet
          ) { root =>
            root.innerNodeTraverser().withParameters(Dfs(AnyConnected)).toSet --
              (if (ignorePredecessors) predecessors(maybeRoot) else Nil)
          }
          val set = seq.toSet
          if (set != expected)
            fail(
              s"Ordering is incomplete when root=$maybeRoot and ignorePredecessors=$ignorePredecessors: expected ${expected} but was ${set}."
            )
        }

        checkOrder(seq, if (ignorePredecessors) root else None)
        checkCompleteness(seq, root, ignorePredecessors)
      }
    }

    def unexpectedCycle[N, E[+X] <: EdgeLikeIn[X]](cycleNode: Option[Graph[N, E]#NodeT]) =
      fail(s"Unexpected cycle starting at $cycleNode")

    def unexpectedRight[N, E[+X] <: EdgeLikeIn[X]](order: Graph[N, E]#TopologicalOrder[_]) =
      fail(s"Cycle expected but topological order ${order.toLayered} found")

    def checkIsCycleNode(g: Graph[Int, DiEdge])(maybeNode: Option[g.NodeT], expectedDefined: Boolean): Unit =
      (maybeNode, expectedDefined) match {
        case (Some(n), true) =>
          g.findCycleContaining(n) orElse fail(s"Cycle containing node $n expected but none found.")
        case (Some(n), false) => fail(s"Unexpected cycle node $n found.")
        case (None, true)     => fail(s"Cycle node expected but None found.")
        case (None, false)    =>
      }
  }

  def `empty graph`(): Unit =
    given(factory.empty[Int, DiEdge]) {
      _.topologicalSort.fold(
        Topo.unexpectedCycle,
        _ should be('empty)
      )
    }

  def `daily activities`(): Unit = {

    object Activities {
      val (coffee, coding, inspiration, shopping, sleeping, supper, gaming) =
        ('coffee, 'coding, 'inspiration, 'shopping, 'sleeping, 'supper, 'gaming)
      val (making_music, driving_to_work, driving_home, listening_to_music) =
        (Symbol("making music"), Symbol("driving to work"), Symbol("driving home"), Symbol("listening to music"))
    }
    import Activities._

    val typicalDay = factory[Symbol, DiEdge](
      coffee ~> coding,
      inspiration ~> coding,
      shopping ~> coffee,
      coding ~> sleeping,
      supper ~> sleeping,
      gaming ~> sleeping,
      making_music ~> sleeping,
      inspiration ~> making_music,
      shopping ~> supper,
      driving_home ~> supper,
      driving_home ~> sleeping,
      coding ~> driving_home,
      driving_to_work ~> coding,
      driving_to_work ~> driving_home,
      driving_home ~> gaming,
      listening_to_music
    )

    given(typicalDay) {
      _.topologicalSort.fold(
        Topo.unexpectedCycle,
        order =>
          new Topo.Checker(typicalDay) {
            checkOuterNodes(order.toOuter)
          }
      )
    }
  }

  def `connected graph`(): Unit = {
    val someOuter @ (n0 :: n1 :: n5 :: Nil) = 0 :: 1 :: 5 :: Nil
    given(factory[Int, DiEdge](n0 ~> n1, 2 ~> 4, 2 ~> n5, n0 ~> 3, n1 ~> 4, 4 ~> 3)) { g =>
      g should not be 'isMulti
      g.topologicalSort.fold(
        Topo.unexpectedCycle,
        order =>
          new Topo.Checker(g) {
            checkOuterNodes(order.toOuter)
            for {
              outer <- someOuter
              inner = graph get outer
              ignorePredecessors <- Array(false, true)
            } inner
              .topologicalSort(ignorePredecessors)
              .fold(Topo.unexpectedCycle, order => checkInnerNodes(order, Some(inner), ignorePredecessors))
          }
      )
    }
  }

  def `multi graph`(): Unit =
    given(factory(WkDiEdge(1, 2)(0), WkDiEdge(1, 2)(1))) { g =>
      g.topologicalSort.fold(
        Topo.unexpectedCycle,
        order =>
          new Topo.Checker(g) {
            checkOuterNodes(order.toOuter)
          }
      )
    }

  def `unconnected graph`(): Unit = {
    val expectedLayer_0 @ (_1 :: _3 :: Nil) = List(1, 3)
    val expectedLayer_1 @ (_2 :: _4 :: Nil) = List(2, 4)
    given(factory(_1 ~> _2, _3 ~> _4)) {
      _.topologicalSort.fold(
        Topo.unexpectedCycle,
        _.toLayered.toOuter.toList match {
          case (layer_0 :: layer_1 :: Nil) =>
            layer_0._2.toList.sorted should be(expectedLayer_0)
            layer_1._2.toList.sorted should be(expectedLayer_1)
          case _ => fail
        }
      )
    }
  }

  def `minimal cyclic graph`(): Unit =
    given(factory(1 ~> 2, 2 ~> 1)) { g =>
      g.topologicalSort.fold(
        Topo.checkIsCycleNode(g)(_, expectedDefined = false),
        Topo.unexpectedRight
      )
    }

  def `cyclic graph #68`(): Unit =
    given(factory(0 ~> 7, 4 ~> 7, 7 ~> 3, 3 ~> 4, 0 ~> 5)) { g =>
      g.topologicalSort.fold(
        Topo.checkIsCycleNode(g)(_, expectedDefined = true),
        Topo.unexpectedRight
      )
    }

  def `cyclic graphs #264`(): Unit =
    given(
      factory(
        111 ~> 2,
        2 ~> 111,
        111 ~> 33
      ) ++ (for {
        i <- Range.inclusive(33, 230, step = 10)
        j = i + 10
      } yield i ~> j)
    ) { g =>
      g.topologicalSort.fold(
        Topo.checkIsCycleNode(g)(_, expectedDefined = false),
        Topo.unexpectedRight
      )
    }

  def `cyclic unconnected graph`(): Unit =
    given(factory(11100 ~> 2, 6 ~> 7, 2 ~> 11100, 3 ~> 4)) { g =>
      g.topologicalSort.fold(
        Topo.checkIsCycleNode(g)(_, expectedDefined = false),
        Topo.unexpectedRight
      )
    }

  def `cyclic unconnected graph by component`(): Unit =
    given(factory(11100 ~> 2, 6 ~> 7, 2 ~> 11100, 3 ~> 4)) { g =>
      val r = g.topologicalSortByComponent
      r.size shouldBe 3
      r.count(_.isLeft) shouldBe 1
    }

  def `combining with filtered edges by withSubgraph #104`(): Unit =
    given(factory((1 ~+> 3)("a"), (1 ~+> 2)("b"), (2 ~+> 3)("a"))) { g =>
      val n1 = g get 1
      n1.topologicalSort() should be('isRight)

      n1.withSubgraph(edges = _.label == "a")
        .topologicalSort()
        .fold(
          Topo.unexpectedCycle,
          order =>
            new Topo.Checker(g) {
              checkOuterNodes(order.toOuter)
            }
        )
    }
}
