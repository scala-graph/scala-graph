package scalax.collection

import scala.util.chaining._

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.immutable.{Graph, TypedGraphFactory}

class MappingTypedSpec extends RefSpec with Matchers {

  object `when mapping a typed graph you may` {
    import MappingTypedSpec._
    import TGraph.OuterImplicits._

    private val a_1   = A(1)
    private val b_0_0 = B(0, 0)

    def `map node values without changing node or edge types`: Unit =
      () /*
      TGraph(Connector(a_1, b_0_0)) pipe { g =>
        g.mapBounded { (n: g.NodeT) =>
          n match {
            case g.InnerNode(_, a: A) => a.copy(a.a + 1)
            case g.InnerNode(_, b: B) => b.copy(b.a + 1, b.b + 1)
          }
        } shouldEqual TGraph(Connector(A(2), B(1, 1)))
      }
       */

    def `downcast nodes`: Unit =
      () /*
      TGraph(Connector(a_1, b_0_0)) pipe { g =>
        g.mapBounded(_ => b_0_0) should not be empty
      }
       */

    def `not upcast nodes without passing an edge mapper`: Unit =
      TGraph(AConnector(a_1, a_1)) pipe { _ =>
        "g.map(_ => b_0_0): Graph[B, Edge]" shouldNot compile
        "g.map(_.toString)" shouldNot compile
      }

    def `upcast nodes to another typed edge if the typed edge mapper is passed`: Unit =
      TGraph(AConnector(a_1, a_1)) pipe { g =>
        g.mapBounded[Node, Connector](_ => b_0_0, Connector(_, _)) pipe { mapped =>
          mapped.edges.head.outer should ===(Connector(b_0_0, b_0_0))
        }
      }

    def `upcast nodes to any type if a generic edge mapper is passed`: Unit =
      () /* TODO parametrize TypedGraphFactory.apply to allow infering TGraph[A, AConnector]
      TGraph(AConnector(a_1, a_1)) pipe { g =>
        def toString(a: A): String = s"""string-$a"""

        val mapped = g.map(n => toString(n.outer), UnDiEdge[String] _)
        mapped.size shouldBe 1
        mapped.edges.head.outer shouldBe (toString(A(1)) ~ toString(A(1)))
      }
       */
  }
}

private object MappingTypedSpec {
  private trait Node
  private case class A(a: Int)         extends Node
  private case class B(a: Int, b: Int) extends Node

  sealed abstract private class Edge(source: Node, target: Node)
      extends AbstractDiEdge[Node](source, target)
      with PartialMapper

  private case class Connector(override val source: Node, override val target: Node)
      extends Edge(source, target)
      with PartialEdgeMapper[Connector] {
    def map[N]: PartialFunction[(N, N), Connector] = { case (source: Node, target: Node) =>
      copy(source, target)
    }
  }

  private case class AConnector(override val source: A, override val target: A)
      extends Edge(source, target)
      with PartialEdgeMapper[AConnector] {
    def map[N]: PartialFunction[(N, N), AConnector] = { case (source: A, target: A) =>
      copy(source, target)
    }
  }

  private object TGraph extends TypedGraphFactory[Node, Edge]
}
