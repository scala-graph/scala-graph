package scalax.collection

import scala.language.higherKinds

import GraphPredef._, GraphEdge._
import GraphTraversal._, GraphTraversal.Parameters._
import generic.GraphCoreCompanion
import edge.Implicits._

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.refspec.RefSpec

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.edge.WkDiEdge
import scalax.collection.visualization.Visualizer

@RunWith(classOf[JUnitRunner])
class TTopologicalSortRootTest
    extends Suites(
      new TTopologicalSort[immutable.Graph](immutable.Graph),
      new TTopologicalSort[mutable.Graph](mutable.Graph)
    )

private class TTopologicalSort[G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]](
    val factory: GraphCoreCompanion[G])
    extends RefSpec
    with Matchers
    with PropertyChecks
    with Visualizer[G] {

  private object Topo {

    class Checker[N, E[X] <: EdgeLikeIn[X]](val graph: G[N, E]) {

      def checkOuterNodes(seq: Traversable[N]): Unit =
        checkInnerNodes(seq map (graph get _))

      type OrderedInnerNodes = Traversable[graph.NodeT]

      def checkInnerNodes(seq: OrderedInnerNodes,
                          root: Option[graph.NodeT] = None,
                          ignorePredecessors: Boolean = false): Unit = {
        checkOrder(seq, if (ignorePredecessors) root else None)
        checkCompletenis(seq, root, ignorePredecessors)
      }

      private def predecessors(maybeRoot: Option[graph.NodeT]): Set[graph.NodeT] = maybeRoot.fold(
        ifEmpty = Set.empty[graph.NodeT]
      ) { root =>
        root.innerNodeTraverser().withParameters(Dfs(Predecessors)).toSet - root
      }

      def checkOrder(seq: OrderedInnerNodes, ignorePredecessorsOf: Option[graph.NodeT]): Unit =
        (predecessors(ignorePredecessorsOf) /: seq) { (allowedPredecessors, innerNode) =>
          if (!innerNode.diPredecessors.forall(allowedPredecessors.contains))
            fail(s"$innerNode is misplaced in $seq")
          allowedPredecessors + innerNode
        }

      def checkCompletenis(seq: OrderedInnerNodes,
                           maybeRoot: Option[graph.NodeT],
                           ignorePredecessors: Boolean): Unit = {
        val expected = maybeRoot.fold(
          ifEmpty = graph.nodes.toSet
        ) { root =>
          root.innerNodeTraverser().withParameters(Dfs(AnyConnected)).toSet --
            (if (ignorePredecessors) predecessors(maybeRoot) else Nil)
        }
        val set = seq.toSet
        if (set != expected)
          fail(
            s"Ordering is incomplete when root=$maybeRoot and ignorePredecessors=$ignorePredecessors: expected ${expected} but was ${set}.")
      }
    }

    def unexpectedCycle[N, E[X] <: EdgeLikeIn[X]](cycleNode: Graph[N, E]#NodeT) =
      fail(s"Unexpected cycle starting at ${cycleNode.value}")

    def unexpectedRight[N, E[X] <: EdgeLikeIn[X]](order: Graph[N, E]#TopologicalOrder[_]) =
      fail(s"Cycle expected but topological order ${order.toLayered} found")
  }

  def `empty graph` {
    given(factory.empty[Int, DiEdge]) {
      _.topologicalSort.fold(
        Topo.unexpectedCycle,
        _ should be('empty)
      )
    }
  }

  def `daily activities` {

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

  def `connected graph` {
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
            } {
              inner
                .topologicalSort(ignorePredecessors)
                .fold(Topo.unexpectedCycle, order => checkInnerNodes(order, Some(inner), ignorePredecessors))
            }
        }
      )
    }
  }

  def `multi graph` {
    given(factory(WkDiEdge(1, 2)(0), WkDiEdge(1, 2)(1))) { g =>
      g.topologicalSort.fold(
        Topo.unexpectedCycle,
        order =>
          new Topo.Checker(g) {
            checkOuterNodes(order.toOuter)
        }
      )
    }
  }

  def `unconnected graph` {
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

  def `cyclic graph` {
    given(factory(1 ~> 2, 2 ~> 1)) {
      _.topologicalSort.fold(
        identity,
        Topo.unexpectedRight
      )
    }
  }

  def `cyclic graph #68` {
    given(factory(0 ~> 7, 4 ~> 7, 7 ~> 3, 3 ~> 4, 0 ~> 5)) {
      _.topologicalSort.fold(
        identity,
        Topo.unexpectedRight
      )
    }
  }

  def `combining with filtered edges by withSubgraph #104` {
    given(factory((1 ~+> 3)("a"), (1 ~+> 2)("b"), (2 ~+> 3)("a"))) { g =>
      val n1 = (g get 1)
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
}
