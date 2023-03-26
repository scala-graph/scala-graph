package scalax.collection

import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scalax.collection.OuterImplicits._
import scalax.collection.GraphTraversal.Parameters._
import scalax.collection.GraphTraversal._
import scalax.collection.edges._
import scalax.collection.generic.{Edge, GenericGraphCoreFactory}
import scalax.collection.visualization.Visualizer

class TopologicalSortSpec
    extends Suites(
      new TopologicalSort[immutable.Graph](immutable.Graph),
      new TopologicalSort[mutable.Graph](mutable.Graph)
    )

final private class TopologicalSort[G[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, G]](
    val factory: GenericGraphCoreFactory[G]
) extends RefSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with IntelliJ[G]
    with Visualizer {

  private object Topo {

    class Checker[N, E <: Edge[N]](val graph: AnyGraph[N, E]) {

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

    def unexpectedCycle[N, E <: Edge[N]](cycleNode: Option[AnyGraph[N, E]#NodeT]): Nothing =
      fail(s"Unexpected cycle with contained node $cycleNode")

    def unexpectedRight[N, E <: Edge[N]](order: AnyGraph[N, E]#TopologicalOrder[_]): Nothing =
      fail(s"Cycle expected but topological order ${order.toLayered} found")

    def checkIsCycleNode(g: AnyGraph[Int, DiEdge[Int]])(maybeNode: Option[g.NodeT]): Unit = maybeNode match {
      case Some(n) => g.findCycleContaining(n) orElse fail(s"Cycle containing node $n expected but none found.")
      case None =>
        g.nodes.find(_.inDegree == 0) match {
          case Some(startingNode) => fail(s"No node with inDegree == 0 expected but node $startingNode found.")
          case None               =>
        }
    }
  }

  def `empty graph`(): Unit =
    withGraph(factory.empty[Int, DiEdge[Int]].asAnyGraph) {
      _.topologicalSort.fold(
        Topo.unexpectedCycle,
        _ shouldBe empty
      )
    }

  def `daily activities`(): Unit = {

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
    ).asAnyGraph

    withGraph(typicalDay) {
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
    val someOuter @ n0 :: n1 :: n5 :: Nil = 0 :: 1 :: 5 :: Nil
    val connected = factory[Int, DiEdge](n0 ~> n1, 2 ~> 4, 2 ~> n5, n0 ~> 3, n1 ~> 4, 4 ~> 3).asAnyGraph
    withGraph(connected) { g =>
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

  def `multi graph`(): Unit = {
    import scalax.collection.edges.multilabeled._

    val g = factory(1 ~> 2 %% 0, 1 ~> 2 %% 1).asAnyGraph

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
    withGraph(factory(_1 ~> _2, _3 ~> _4)) {
      _.topologicalSort.fold(
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

  def `minimal cyclic graph`(): Unit =
    withGraph(factory(1 ~> 2, 2 ~> 1)) { g =>
      g.topologicalSort.fold(
        Topo.checkIsCycleNode(g),
        Topo.unexpectedRight
      )
    }

  def `cyclic graph #68`(): Unit =
    withGraph(factory(0 ~> 7, 4 ~> 7, 7 ~> 3, 3 ~> 4, 0 ~> 5)) {
      _.topologicalSort.fold(
        identity,
        Topo.unexpectedRight
      )
    }

  def `cyclic graphs #264`(): Unit =
    withGraph(
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
        Topo.checkIsCycleNode(g),
        Topo.unexpectedRight
      )
    }

  /* TODO L
  def `combining with filtered edges by withSubgraph #104`: Unit =
    given(factory((1 ~+> 3)("a"), (1 ~+> 2)("b"), (2 ~+> 3)("a")).asAnyGraph) { g =>
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
