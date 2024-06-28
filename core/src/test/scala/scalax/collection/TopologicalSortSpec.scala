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
              s"Ordering is incomplete when root=$maybeRoot and ignorePredecessors=$ignorePredecessors: expected $expected but was $set."
            )
        }

        checkOrder(seq, if (ignorePredecessors) root else None)
        checkCompleteness(seq, root, ignorePredecessors)
      }
    }

    def unexpectedCycle[N, E <: Edge[N]](cycleNode: AnyGraph[N, E]#TopologicalSortFailure): Nothing =
      fail(s"Unexpected cycle with candidate cycle nodes $cycleNode")

    def unexpectedRight[N, E <: Edge[N]](order: AnyGraph[N, E]#TopologicalOrder[_]): Nothing =
      fail(s"Cycle expected but topological order ${order.toLayered} found")

    def checkCycleHint(
        g: AnyGraph[Int, DiEdge[Int]]
    )(hint: g.TopologicalSortFailure, expectedDefined: Boolean): Unit =
      (hint.candidateCycleNodes, expectedDefined) match {
        case (ns, true) if ns.nonEmpty =>
          hint.cycle orElse fail(s"Cycle containing any of $ns expected but none found.")
        case (ns, false) if ns.nonEmpty => fail(s"Unexpected cycle node hints $ns found.")
        case (_, true)                  => fail(s"Non-empty cycle node hint expected.")
        case (_, false)                 =>
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
        Topo.checkCycleHint(g)(_, expectedDefined = false),
        Topo.unexpectedRight
      )
    }

  def `cyclic graph #68`(): Unit =
    withGraph(factory(0 ~> 7, 4 ~> 7, 7 ~> 3, 3 ~> 4, 0 ~> 5)) { g =>
      g.topologicalSort.fold(
        Topo.checkCycleHint(g)(_, expectedDefined = true),
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
        Topo.checkCycleHint(g)(_, expectedDefined = false),
        Topo.unexpectedRight
      )
    }

  def `cyclic unconnected graph`(): Unit =
    withGraph(factory(11100 ~> 2, 6 ~> 7, 2 ~> 11100, 3 ~> 4)) { g =>
      g.topologicalSort.fold(
        Topo.checkCycleHint(g)(_, expectedDefined = false),
        Topo.unexpectedRight
      )
    }

  def `cyclic unconnected graph by component`(): Unit =
    withGraph(factory(11100 ~> 2, 6 ~> 7, 2 ~> 11100, 3 ~> 4)) { g =>
      val r = g.topologicalSortByComponent
      r.size shouldBe 3
      r.count(_.isLeft) shouldBe 1
    }

  def `proper cycle node out of multiple hints`(): Unit =
    withGraph(factory(0 ~> 11, 0 ~> 20, 1 ~> 20, 11 ~> 20, 11 ~> 30, 30 ~> 11)) { g =>
      g.topologicalSort.fold(
        Topo.checkCycleHint(g)(_, expectedDefined = true),
        Topo.unexpectedRight
      )
    }

  def `with filtered edges #104`(): Unit = {
    import scalax.collection.edges.labeled._

    withGraph(factory(1 ~> 3 % 1, 1 ~> 2 % 2, 2 ~> 3 % 1)) { g =>
      val n1 = g get 1
      n1.topologicalSort().isRight shouldBe true

      n1.withSubgraph(edges = _.weight == 1)
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
