package scalax.collection

import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scalax.collection.GraphPredef._
import scalax.collection.GraphTraversal.Parameters._
import scalax.collection.GraphTraversal._
import scalax.collection.edges._
import scalax.collection.generic.{Edge, GraphCoreCompanion}
import scalax.collection.visualization.Visualizer

class TopologicalSortSpec
    extends Suites(
      new TopologicalSort[immutable.Graph](immutable.Graph),
      new TopologicalSort[mutable.Graph](mutable.Graph)
    )

final private class TopologicalSort[G[N, E <: Edge[N]] <: Graph[N, E] with GraphLike[N, E, G]](
    val factory: GraphCoreCompanion[G]
) extends RefSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with Visualizer[G] {

  private object Topo {

    class Checker[N, E <: Edge[N]](val graph: G[N, E]) {

      def checkOuterNodes(seq: Iterable[N]): Unit =
        checkInnerNodes(seq map (graph get _))

      type OrderedInnerNodes = Iterable[graph.NodeT]

      def checkInnerNodes(
          seq: OrderedInnerNodes,
          root: Option[graph.NodeT] = None,
          ignorePredecessors: Boolean = false
      ): Unit = {
        checkOrder(seq, if (ignorePredecessors) root else None)
        checkCompletenis(seq, root, ignorePredecessors)
      }

      private def predecessors(maybeRoot: Option[graph.NodeT]): Set[graph.NodeT] =
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

      def checkCompletenis(
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
    }

    def unexpectedCycle[N, E <: Edge[N]](cycleNode: Graph[N, E]#NodeT) =
      fail(s"Unexpected cycle starting at ${cycleNode.outer}")

    def unexpectedRight[N, E <: Edge[N]](order: Graph[N, E]#TopologicalOrder[_]) =
      fail(s"Cycle expected but topological order ${order.toLayered} found")
  }

  def `empty graph`: Unit =
    given(factory.empty[Int, DiEdge[Int]]) {
      _.topologicalSort.fold(
        Topo.unexpectedCycle,
        _ shouldBe empty
      )
    }

  def `daily activities`: Unit = {

    object Activities {
      val (coffee, coding, inspiration, shopping, sleeping, supper, gaming) =
        ("coffee", "coding", "inspiration", "shopping", "sleeping", "supper", "gaming")
      val (making_music, driving_to_work, driving_home, listening_to_music) =
        ("making music", "driving to work", "driving home", "listening to music")
    }
    import Activities._

    val typicalDay = factory[String, DiEdge](
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

    given(typicalDay) { case g: Graph[String, DiEdge[String]] => // `annotated for IntelliJ
      g.topologicalSort.fold(
        Topo.unexpectedCycle,
        order =>
          new Topo.Checker(typicalDay) {
            checkOuterNodes(order.toOuter)
          }
      )
    }
  }

  def `connected graph`: Unit = {
    val someOuter @ n0 :: n1 :: n5 :: Nil = 0 :: 1 :: 5 :: Nil
    val connected                         = factory[Int, DiEdge](n0 ~> n1, 2 ~> 4, 2 ~> n5, n0 ~> 3, n1 ~> 4, 4 ~> 3)
    given(connected) { case g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
      g.isMulti shouldBe false
      g.topologicalSort.fold(
        Topo.unexpectedCycle,
        order =>
          new Topo.Checker(connected) {
            checkOuterNodes(order.toOuter)
            for {
              outer <- someOuter
              inner = graph get outer
              ignorePredecessors <- Array(false, true)
            }
              inner
                .topologicalSort(ignorePredecessors)
                .fold(Topo.unexpectedCycle, order => checkInnerNodes(order, Some(inner), ignorePredecessors))
          }
      )
    }
  }

  /* TODO L
  def `multi graph`: Unit = {
    given(factory(WkDiEdge(1, 2)(0), WkDiEdge(1, 2)(1))) { g =>
      g.topologicalSort.fold(
        Topo.unexpectedCycle,
        order =>
          new Topo.Checker(g) {
            checkOuterNodes(order.outer)
          }
      )
    }
  }
   */

  def `unconnected graph`: Unit = {
    val expectedLayer_0 @ (_1 :: _3 :: Nil) = List(1, 3)
    val expectedLayer_1 @ (_2 :: _4 :: Nil) = List(2, 4)
    given(factory(_1 ~> _2, _3 ~> _4)) { g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
      g.topologicalSort.fold(
        Topo.unexpectedCycle,
        _.toLayered.toOuter.toList match {
          case (layer_0 :: layer_1 :: Nil) =>
            layer_0._2.toList.sorted should be(expectedLayer_0)
            layer_1._2.toList.sorted should be(expectedLayer_1)
          case _ => fail()
        }
      )
    }
  }

  def `cyclic graph`: Unit =
    given(factory(1 ~> 2, 2 ~> 1)) { g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
      g.topologicalSort.fold(
        identity,
        Topo.unexpectedRight
      )
    }

  def `cyclic graph #68`: Unit =
    given(factory(0 ~> 7, 4 ~> 7, 7 ~> 3, 3 ~> 4, 0 ~> 5)) { g: Graph[Int, DiEdge[Int]] => // `annotated for IntelliJ
      g.topologicalSort.fold(
        identity,
        Topo.unexpectedRight
      )
    }

  /* TODO L
  def `combining with filtered edges by withSubgraph #104`: Unit =
    given(factory((1 ~+> 3)("a"), (1 ~+> 2)("b"), (2 ~+> 3)("a"))) { g: Graph[Int, ???[Int]] => // `annotated for IntelliJ
      val n1 = g get 1
      n1.topologicalSort() shouldBe Right

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
   */
}
