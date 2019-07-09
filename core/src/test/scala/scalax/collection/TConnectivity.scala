package scalax.collection

import scala.language.{higherKinds, postfixOps}
import scala.util.Random

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion
import edge.Implicits._
import generator._, RandomGraph._

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.visualization.Visualizer

@RunWith(classOf[JUnitRunner])
class TConnectivityRootTest
    extends Suites(
      new TConnectivity[immutable.Graph](immutable.Graph),
      new TConnectivity[mutable.Graph](mutable.Graph)
    )

final class TConnectivity[G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]](
    val factory: GraphCoreCompanion[G])
    extends RefSpec
    with Matchers
    with PropertyChecks
    with Visualizer[G] {

  implicit val config = PropertyCheckConfiguration(minSuccessful = 5, maxDiscardedFactor = 1.0)

  object `In a weakly connected diGraph` {
    import Data.elementsOfDi_1
    val g = factory(elementsOfDi_1: _*)

    def `there exists no pair of mutually reachable nodes` {
      given(g) {
        _.nodes.toList.combinations(2) foreach {
          case List(a, b) => List(a pathTo b, b pathTo a) should contain(None)
        }
      }
    }

    def `evaluating strong components from any node yields single-node components` {
      given(g) {
        _.nodes foreach { n =>
          val components = n.innerNodeTraverser.strongComponents
          components foreach (_.nodes should have size (1))
        }
      }
    }

    def `evaluating all strong components yields a component for every node` {
      given(g) { g =>
        g.strongComponentTraverser().size should be(g.order)
      }
    }
  }

  object `Having two strong components` {
    // see example on https://de.wikipedia.org/wiki/Algorithmus_von_Tarjan_zur_Bestimmung_starker_Zusammenhangskomponenten
    val sccExpected = Vector[G[Symbol, DiEdge]](
      factory('a ~> 'b, 'b ~> 'c, 'c ~> 'd, 'd ~> 'a, 'd ~> 'e, 'c ~> 'e, 'e ~> 'c),
      factory('f ~> 'g, 'g ~> 'f, 'g ~> 'h, 'h ~> 'j, 'j ~> 'i, 'i ~> 'g, 'i ~> 'f, 'f ~> 'i)
    )
    assert(sccExpected.size == 2)
    assert(sccExpected(0).intersect(sccExpected(1)) == factory.empty)

    def `each is detected as such` {
      sccExpected foreach (g =>
        given(g) {
          _.strongComponentTraverser() should have size (1)
        })
    }

    def `connected by a diEdge yields a graph with the very same two strong components` {
      val r     = new Random
      val union = (factory.empty[Symbol, DiEdge] /: sccExpected)((r, g) => g union r)
      val connectors = {
        def pickNode(index: Int) = sccExpected(index).nodes.draw(r).toOuter
        for (i <- 1 to 10) yield pickNode(0) ~> pickNode(1)
      }
      connectors foreach { connector =>
        val connected = union + connector
        def check(scc: Traversable[connected.Component], expectedSize: Int): Unit = {
          scc should have size (expectedSize)
          scc foreach { sc =>
            given(sc.to(factory)) { g =>
              sccExpected should contain(g)
              sc.frontierEdges should have size (1)
            }
          }
        }

        check(connected.strongComponentTraverser().toVector, 2)

        val start = connected.nodes.draw(r)
        check(
          start.innerNodeTraverser.strongComponents.toVector,
          if (sccExpected(0) contains start) 2 else 1
        )
      }
    }
  }

  object `Having two weak components` {

    def `weak components are detected, fix #57` {
      given(factory(11 ~> 12, 13 ~> 14)) { _.componentTraverser() should have size 2 }
    }
  }

  object `Having a bigger graph` {
    val g: G[Int, DiEdge] = {
      val gOrder = 1000
      val random = RandomGraph.diGraph(factory, new IntFactory {
        val order       = gOrder
        val nodeDegrees = NodeDegreeRange(gOrder / 10, gOrder / 4)
      })
      random.draw
    }
    lazy val strongComponents = g.strongComponentTraverser().toVector

    def `no stack overflow occurs` {
      given(g) { _ =>
        strongComponents
      }
    }

    def `strong components are complete` {
      given(g) { _ =>
        (Set.empty[g.NodeT] /: strongComponents)((cum, sc) => cum ++ sc.nodes) should be(g.nodes)
      }
    }

    def `strong components are proper` {
      given(g) { _ =>
        val maxProbes = 10
        val arbitraryNodes: Vector[Set[g.NodeT]] = strongComponents map { sc =>
          val nodes = sc.nodes
          if (nodes.size <= maxProbes) nodes
          else {
            val every = nodes.size / maxProbes
            (nodes zipWithIndex) withFilter { case (n, i) => i % every == 0 } map (_._1)
          }
        }
        arbitraryNodes foreach {
          case nodes =>
            def checkBiConnected(n1: g.NodeT, n2: g.NodeT) =
              if (n1 ne n2) {
                n1 pathTo n2 should be('isDefined)
                n2 pathTo n1 should be('isDefined)
              }

            nodes.sliding(2) foreach { pairOrSingle =>
              pairOrSingle.toList match {
                case List(n1, n2) => checkBiConnected(n1, n2)
                case n :: Nil     => checkBiConnected(n, nodes.head)
              }
            }
        }
        arbitraryNodes.sliding(2) foreach { pairOrSingle =>
          def checkNonBiConnected(ns1: Set[g.NodeT], ns2: Set[g.NodeT]) =
            if (ns1 ne ns2) {
              ns1 zip ns2 foreach {
                case (n1, n2) =>
                  (n1 pathTo n2).isDefined &&
                    (n2 pathTo n1).isDefined should be(false)
              }
            }

          pairOrSingle.toList match {
            case List(ns1, ns2) => checkNonBiConnected(ns1, ns2)
            case ns :: Nil      => checkNonBiConnected(ns, arbitraryNodes.head)
          }
        }
      }
    }
  }
}
