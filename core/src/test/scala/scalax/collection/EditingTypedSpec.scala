package scalax.collection

import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites
import org.scalatest.refspec.RefSpec

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.generic.GraphCoreCompanion
import scalax.collection.visualization.Visualizer

class EditingTypedSpec
    extends Suites(
      new EditingTyped[scalax.collection.immutable.Graph](scalax.collection.immutable.Graph),
      new EditingTyped[scalax.collection.mutable.Graph](scalax.collection.mutable.Graph)
    )

private class EditingTyped[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers
    with Visualizer[CC] {

  import scalax.collection.edges.Aviation
  import Aviation._
  import scalax.collection.edges.Flight

  val (ham, gig) = (Airport("HAM"), Airport("GIG"))
  val flightNo   = "LH007"

  object `Custom edge 'Flight'` {
    def `proper methods` :Unit = {
      import Aviation.Implicits._

      val outer = Flight(ham, gig, flightNo)

      // TODO get apply/from work with Flight
      given(factory.from[Airport, Flight](Nil, outer :: Nil)) { g =>
        val e = g.edges.head
        e.ends.head.getClass should be(g.nodes.head.getClass)
        e.departure should be(ham)
        e.destination should be(gig)
        e.flightNo should be(flightNo)
        e should be(outer)
        e.## should be(outer.##)
        val eqFlight = edges.Flight(ham, gig, flightNo, 11 o 2)
        e should be(eqFlight)
        e.## should be(eqFlight.##)
        val neFlight = edges.Flight(ham, gig, flightNo + "x", 11 o 2)
        e should not be (neFlight)
        e.## should not be (neFlight.##)
      }
    }

    def `proper method shortcuts` :Unit={
      import Aviation.Implicits._
      import Flight._

      val outer = Flight(ham, gig, flightNo)
      given(factory(outer)) { _ =>
        ham ~> gig ## flightNo should be(outer)
        ham ~> gig ## (flightNo, 11 o 20) should be(outer)
      }
    }
  }
}
