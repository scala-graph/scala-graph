package scalax.collection

import language.{higherKinds, implicitConversions}

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TMapRootTest
    extends Suites(
      new TMap[immutable.Graph](immutable.Graph),
      new TMap[mutable.Graph](mutable.Graph)
    )

class TMap[CC[N, E[X] <: EdgeLike[X]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC]
) extends RefSpec
    with Matchers {

  object `the map of an undirected graph with generic edges` {
    private val edge      = 1 ~ 2
    private val originalG = factory(edge)

    private def fNode(n: originalG.NodeT) = n.outer + 1

    def `yields another graph` {
      val g = originalG map fNode

      g shouldBe a[CC[Int, UnDiEdge] @unchecked]
      g.nodes.head.outer shouldBe an[Integer]
      g.edges.head shouldBe an[g.Inner.UnDiEdge]
      (g.edges.head.outer: UnDiEdge[Int]) shouldBe an[UnDiEdge[_]]
    }
    def `has correctly mapped nodes` {
      val g = originalG map fNode

      originalG.nodes zip g.nodes.toOuter foreach {
        case (original, mapped) => fNode(original) == mapped
      }
    }
    def `has correctly mapped edges` {
      val g = originalG map fNode

      g.edges.head should be(UnDiEdge(2, 3))
    }
    def `may have a new node type` {
      val g = originalG map (_.toString)

      g.nodes.head.outer shouldBe a[String]
      (g.edges.head.outer: UnDiEdge[String]) shouldBe an[UnDiEdge[_]]
      g.edges.head should be(edge._1.toString ~ edge._2.toString)
    }
  }

  object `when mapping a directed typed graph you may` {

    private trait Node
    private case class A(a: Int)         extends Node
    private case class B(a: Int, b: Int) extends Node

    private object edges {
      trait Edge[+N, +This <: AbstractEdge[N]] extends AbstractDiEdge[N] with PartialEdgeMapper[N, This]

      case class NodeConnector(source: Node, target: Node) extends Edge[Node, NodeConnector] {
        def map[NN]: PartialFunction[(NN, NN), NodeConnector] = {
          case (node_1: Node, node_2: Node) => copy(node_1, node_2)
        }
      }

      case class AConnector(source: A, target: A) extends Edge[A, AConnector] {
        def map[NN]: PartialFunction[(NN, NN), AConnector] = {
          case (node_1: A, node_2: A) => copy(node_1, node_2)
        }
      }
    }

    // Define aliases to typed edges to be able to pass them to `Graph` that requires edges with a type parameter
    private type Edge[+N]          = AbstractDiEdge[N] with edges.Edge[N, _]
    private type NodeConnector[+N] = AbstractDiEdge[N] with edges.NodeConnector
    private type AConnector[+N]    = AbstractDiEdge[N] with edges.AConnector
    private val NodeConnector = edges.NodeConnector
    private val AConnector    = edges.AConnector

    // Necessary for `Graph.apply`. You may omit this implicit and use `Graph.from` instead.
    @inline implicit private def edgeToOuterEdge[N](e: Edge[N]): OuterEdge[N, Edge] = OuterEdge(e)

    private val a_1 = A(1)

    private val b_0_0 = B(0, 0)

    def `downcast nodes` {
      factory(NodeConnector(a_1, b_0_0)) pipe { g =>
        (g.mapBounded(_ => b_0_0): CC[B, Edge]) should not be empty
      }
    }

    def `not upcast nodes without passing an edge mapper` {
      factory(AConnector(a_1, a_1)) pipe { g =>
        "g.map(_ => b_0_0): Graph[B, Edge]" shouldNot compile
        "g.map(_.toString)" shouldNot compile
      }
    }

    def `upcast nodes to another typed edge if the typed edge mapper is passed` {
      factory(AConnector(a_1, a_1)) pipe { g =>
        g.map[Node, Edge](_ => b_0_0, NodeConnector) pipe { mapped: CC[Node, Edge] =>
          mapped.edges.head.outer should ===(NodeConnector(b_0_0, b_0_0))
        }
      }
    }

    def `upcast nodes to any type if a generic edge mapper is passed` {
      factory(AConnector(a_1, a_1)) pipe { g =>
        def toString(a: A): String = s"""string-$a"""

        def expect[E[X] <: EdgeLike[X]](mapped: CC[String, E], edge: E[String]): Unit = {
          mapped.size should ===(1)
          mapped.edges.head.outer should ===(edge)
        }

        expect(g.map(toString(_), DiEdge[String]), toString(a_1) ~> toString(A(1)))
        expect(g.map(toString(_), UnDiEdge[String]), toString(a_1) ~ toString(A(1)))
      }
    }
  }
}
