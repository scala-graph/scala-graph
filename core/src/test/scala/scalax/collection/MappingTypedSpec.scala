package scalax.collection

import scala.language.implicitConversions
import scala.util.chaining._

import org.scalatest.Suites
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.generic.GenericGraphCoreFactory

class MappingTypedSpec
    extends Suites(
      new MappingTyped[immutable.Graph](immutable.Graph),
      new MappingTyped[mutable.Graph](mutable.Graph)
    )

private class MappingTyped[+CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers {

  object `when mapping a typed graph you may` {
    private trait Node
    private case class A(a: Int)         extends Node { def +(addend: Int): A = A(a + addend)             }
    private case class B(a: Int, b: Int) extends Node { def +(addend: Int): B = B(a + addend, b + addend) }

    private object edges {
      type Connector[+N] = Edge[N, _]

      sealed abstract protected class Edge[+N, +CC <: Edge[N, CC]](override val source: N, override val target: N)
          extends AbstractDiEdge[N](source, target)
          with PartialEdgeMapper[CC]

      case class NodeConnector(override val source: Node, override val target: Node)
          extends Edge[Node, NodeConnector](source, target) {
        def map[N]: PartialFunction[(N, N), NodeConnector] = { case (node_1: Node, node_2: Node) =>
          copy(node_1, node_2)
        }
      }

      case class AConnector(override val source: A, override val target: A)
          extends Edge[A, AConnector](source, target) {
        def map[N]: PartialFunction[(N, N), AConnector] = { case (node_1: A, node_2: A) =>
          copy(node_1, node_2)
        }
      }

      // Necessary for `Graph.apply`. You may omit this implicit and use `Graph.from` instead.
      @inline implicit def edgeToOuterEdge[N](e: Connector[N]): OuterEdge[N, Connector[N]] = OuterEdge(e)
    }

    import edges._

    private val a_1   = A(1)
    private val b_0_0 = B(0, 0)

    def `map node values without changing node or edge types`: Unit =
      factory(NodeConnector(a_1, b_0_0)) pipe { g =>
        g.mapBounded {
          case g.InnerNode(_, a: A) => a + 1
          case g.InnerNode(_, b: B) => b + 1
        } shouldEqual factory(NodeConnector(a_1 + 1, b_0_0 + 1))
      }

    def `downcast nodes`: Unit =
      factory(NodeConnector(a_1, b_0_0)) pipe { g =>
        (g.mapBounded(_ => b_0_0): CC[B, Connector[B]]) should not be empty
      }

    def `not upcast nodes without passing an edge mapper`: Unit =
      factory(AConnector(a_1, a_1)) pipe { g =>
        "g.map(_ => b_0_0): Graph[B, Edge]" shouldNot compile
        "g.map(_.toString)" shouldNot compile
      }

    def `upcast nodes to another typed edge if the typed edge mapper is passed`: Unit =
      factory(AConnector(a_1, a_1)) pipe { g =>
        g.mapBounded[Node, NodeConnector](_ => b_0_0, NodeConnector) pipe { mapped: CC[Node, NodeConnector] =>
          mapped.edges.head.outer should ===(NodeConnector(b_0_0, b_0_0))
        }
      }

    def `upcast nodes to any type if a generic edge mapper is passed`: Unit =
      factory(AConnector(a_1, a_1)) pipe { g =>
        def toString(a: A): String = s"""string-$a"""

        def expect[E[N] <: Edge[N]](mapped: CC[String, E[String]], edge: E[String]): Unit = {
          mapped.size should ===(1)
          mapped.edges.head.outer should ===(edge)
        }

        expect(g.map(toString(_), DiEdge[String]), toString(a_1) ~> toString(A(1)))
        expect(g.map(toString(_), UnDiEdge[String]), toString(a_1) ~ toString(A(1)))
      }
  }
}
