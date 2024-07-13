package scalax.collection

import scala.language.implicitConversions
import scala.util.chaining.*

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.edges.*
import scalax.collection.generic.*
import scalax.collection.immutable.{Graph, TypedGraphFactory}

class MappingTypedSpec extends RefSpec with Matchers {

  object `mapping a typed graph you can` {
    import MappingTypedSpec.*
    import TGraph.OuterImplicits.*

    private val a_1   = A(1)
    private val b_0_0 = B(0, 0)

    def `map nodes`: Unit =
      TGraph(Connector(a_1, b_0_0)) pipe { g =>
        g.mapBound { (n: g.NodeT) =>
          n match {
            case g.InnerNode(_, a: A) => a.copy(a.a + 1)
            case g.InnerNode(_, b: B) => b.copy(b.a + 1, b.b + 1)
          }
        } shouldEqual TGraph(Connector(A(2), B(1, 1)))
      }

    def `downcast nodes`: Unit =
      TGraph(Connector(a_1, b_0_0)) pipe { g =>
        g.mapBound(_ => b_0_0) shouldEqual TGraph(Connector(b_0_0, b_0_0))
      }

    def `not upcast nodes when only passing an node mapper`: Unit =
      TGraph(AConnector(a_1, a_1)) pipe { _ =>
        "g.map(_ => b_0_0): Graph[B, Edge]" shouldNot compile
        "g.map(_.toString)" shouldNot compile
      }

    def `upcast nodes to another typed edge within the ADT`: Unit =
      TGraph(AConnector(a_1, a_1)) pipe { g =>
          g.mapBound[Node, Connector](_ => b_0_0, Connector.apply) pipe { mapped =>
            mapped.edges.head.outer shouldEqual Connector(b_0_0, b_0_0)
          }
      }

    def `upcast nodes to any type if a generic edge mapper is passed`: Unit =
      TGraph(AConnector(a_1, a_1)) pipe { g =>
        def stringify(a: A): String = s"""string-$a"""

        g.map(n => stringify(n.outer), UnDiEdge[String] _) pipe { mapped =>
          mapped.size shouldBe 1
          mapped.edges.head.outer shouldBe (stringify(A(1)) ~ stringify(A(1)))
        }
      }
  }

  object `flat-mapping a typed graph you can` {

    import MappingTypedSpec.*
    import TGraph.OuterImplicits.*

    private val a_1   = A(1)
    private val b_0_0 = B(0, 0)

    private def incr(a: A) = a.copy(a.a + 1)

    private val g = TGraph(
      Connector(A(1), B(0, 0)),
      AConnector(A(1), A(2))
    )

    def `change and add nodes`: Unit =
      g.flatMapBound { (n: g.NodeT) =>
        n match {
          case g.InnerNode(_, a: A) => incr(a) :: a :: Nil
          case g.InnerNode(_, _)    => Nil
        }
      } shouldEqual TGraph(AConnector(A(1), A(2)), A(3))

    def `downcast nodes`: Unit =
      TGraph(Connector(a_1, b_0_0)) pipe { g =>
        g.flatMapBound(_ => b_0_0 :: Nil) shouldEqual TGraph(Connector(b_0_0, b_0_0))
      }

    def `not upcast nodes when only passing an node mapper`: Unit =
      TGraph(AConnector(a_1, a_1)) pipe { _ =>
        "g.flatMap(_ => b_0_0): Graph[B, Edge]" shouldNot compile
        "g.flatMap(_.toString)" shouldNot compile
      }

    def `upcast and change structure within the ADT`: Unit =
      TGraph(AConnector(A(1), A(2))) pipe { g =>
        g.flatMapBound[Node, Connector](
          fNode = n => List(incr(n.outer), b_0_0),
          fEdge = (n1s: Seq[Node], n2s: Seq[Node]) =>
            List(
              Connector(n2s.head, n1s.head),
              Connector(n2s.head, b_0_0)
            )
        ) pipe { mapped =>
          mapped.nodes.outerIterable should contain theSameElementsAs List(A(2), A(3), b_0_0)
          mapped.edges.outerIterable should contain theSameElementsAs List(
            Connector(A(3), A(2)),
            Connector(A(3), b_0_0)
          )
        }
      }

    def `upcast and change structure to any type if a generic edge mapper is passed`: Unit =
      TGraph(AConnector(A(1), A(2))) pipe { g =>
        def stringify(n: Any): String = s"""string-$n"""
        g.flatMap(
          fNode = n => List(stringify(incr(n.outer)), "B"),
          fEdge = (n1s: Seq[String], n2s: Seq[String]) =>
            List(
              n2s.head ~ n1s.head,
              n2s.head ~ "B"
            )
        ) pipe { (mapped: Graph[String, UnDiEdge[String]]) =>
          mapped.nodes.outerIterable should contain theSameElementsAs List(stringify(A(2)), stringify(A(3)), "B")
          mapped.edges.outerIterable should contain theSameElementsAs List(
            stringify(A(3)) ~ stringify(A(2)),
            stringify(A(3)) ~ "B"
          )
        }
      }
  }
}

private object MappingTypedSpec {
  private trait Node
  private case class A(a: Int)         extends Node
  private case class B(a: Int, b: Int) extends Node

  sealed private trait Edge extends AnyDiEdge[Node] with PartialMapper

  private case class Connector(override val source: Node, override val target: Node)
      extends AbstractDiEdge[Node](source, target)
      with PartialEdgeMapper[Connector]
      with Edge {
    def map[N]: PartialFunction[(N, N), Connector] = { case (s: Node, t: Node) => copy(s, t) }
  }

  private case class AConnector(override val source: A, override val target: A)
      extends AbstractDiEdge[A](source, target)
      with PartialEdgeMapper[AConnector]
      with Edge {
    def map[N]: PartialFunction[(N, N), AConnector] = { case (s: A, t: A) => copy(s, t) }
  }

  private object TGraph extends TypedGraphFactory[Node, Edge]

  implicit private def aConnectorToOuterEdge(e: AConnector): OuterEdge[A, AConnector] = OuterEdge(e)
}
