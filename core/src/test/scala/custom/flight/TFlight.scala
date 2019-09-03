package custom.flight

import language.higherKinds

import scalax.collection.{Graph, GraphLike}
import scalax.collection.GraphPredef._
import scalax.collection.generic.GraphCoreCompanion

import org.scalatest._

import org.scalatest.refspec.RefSpec

import Flight.ImplicitEdge, Helper._

import scalax.collection.visualization.Visualizer

class TFlightRootTest
    extends Suites(
      new TFlight[scalax.collection.immutable.Graph](scalax.collection.immutable.Graph),
      new TFlight[scalax.collection.mutable.Graph](scalax.collection.mutable.Graph)
    )

class TFlight[CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers
    with Visualizer[CC] {

  val (ham, gig) = (Airport("HAM"), Airport("GIG"))
  val flightNo   = "LH007"

  object `Custom edge 'Flight'` {
    def `proper methods` {
      val outer = Flight(ham, gig, flightNo)
      given(factory(outer)) { g =>
        val e = g.edges.head
        e.edge.nodes.productElement(0).asInstanceOf[AnyRef].getClass should be(g.nodes.head.getClass)
        e.from should be(ham)
        e.to should be(gig)
        e.flightNo should be(flightNo)
        e should be(outer)
        e.## should be(outer.##)
        val eqFlight = Flight(ham, gig, flightNo, 11 o 2)
        e should be(eqFlight)
        e.## should be(eqFlight.##)
        val neFlight = Flight(ham, gig, flightNo + "x", 11 o 2)
        e should not be (neFlight)
        e.## should not be (neFlight.##)
      }
    }
    def `proper method shortcuts` {
      val outer = Flight(ham, gig, flightNo)
      given(factory(outer)) { _ =>
        ham ~> gig ## flightNo should be(outer)
        ham ~> gig ## (flightNo, 11 o 20) should be(outer)
      }
    }
  }
}
