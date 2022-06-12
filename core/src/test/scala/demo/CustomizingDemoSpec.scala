package demo

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edges.DiEdge
import scalax.collection.generic._

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-customizing.html
  *  Customizing Graphs]].
  */
class CustomizingDemoSpec extends RefSpec with Matchers {

  object `demonstrating how to` {

    def `enrich graphs`: Unit = {
      implicit class ExtGraph[N, E <: Edge[N]](protected val g: Graph[N, E]) {
        def foo: String = "bar"
      }
      val g = Graph(1 ~ 2)
      g.foo shouldBe "bar"
    }

    def `enrich inner nodes`: Unit = {
      // works for any Graph due to projection type
      implicit class ExtGraphNode[N, E <: Edge[N]](node: Graph[N, E]#NodeT) {
        def outOverInDegree: Int = node.outDegree - node.inDegree
      }

      Graph(1 ~> 2).nodes foreach {
        case n if n.outer == 1 => n.outOverInDegree shouldBe 1
        case n if n.outer == 2 => n.outOverInDegree shouldBe -1
        case _                 => fail()
      }
    }

    def `work with a custom edge`: Unit = {
      case class Airport(code: String) {
        override def toString: String = code // without Airport-prefix
      }
      val (ham, ny) = (Airport("HAM"), Airport("JFK"))

      case class Flight[+N](from: N, to: N, flightNo: String, days: Seq[Int])
          extends AbstractGenericDiEdge[N, Flight]
          with ExtendedKey {

        def source: N             = from
        def target: N             = to
        def extendKeyBy: Seq[Any] = Seq(flightNo)

        def map[NN](n_1: NN, n_2: NN): Flight[NN] = copy(n_1, n_2)
      }

      implicit class FlightAssoc[A <: Airport](val e: DiEdge[A]) {
        def ##(flightNo: String, days: Seq[Int]) = new Flight[A](e.source, e.target, flightNo, days)
      }

      val flight = ham ~> ny ## ("007", List(1, 5))
      val g      = Graph(flight)
      g.edges.toOuter shouldBe Set(flight)
    }
  }
}
