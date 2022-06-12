package scalax.collection

import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites
import org.scalatest.refspec.RefSpec

import scalax.collection.generic._
import scalax.collection.GraphPredef._
import scalax.collection.generic.GraphCoreCompanion

class EditingTypedSpec
    extends Suites(
      new EditingTyped[scalax.collection.immutable.Graph](scalax.collection.immutable.Graph),
      new EditingTyped[scalax.collection.mutable.Graph](scalax.collection.mutable.Graph)
    )

private class EditingTyped[CC[N, E <: Edge[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC]
) extends RefSpec
    with Matchers {

  import scalax.collection.edges.Aviation
  import Aviation._
  import scalax.collection.edges.Flight

  val (ham, gig) = (Airport("HAM"), Airport("GIG"))
  val flightNo   = "LH007"

  object `Custom edge 'Flight'` {
    def `edge methods`: Unit = {
      import Aviation.Implicits._

      val outer = Flight(ham, gig, flightNo)

      // TODO get apply/from work with Flight
      val g: Graph[Airport, Flight] = factory.from[Airport, Flight](Nil, outer :: Nil) // `annotated for IntelliJ

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
      e should not be neFlight
      e.## should not be neFlight.##
    }

    def `edge equality`: Unit = {
      val outer_1 = Flight(ham, gig, flightNo)
      val outer_2 = Flight(ham, gig, flightNo + "x")

      outer_1 should not equal outer_2

      val g = factory.from(outer_1 :: outer_2 :: Nil)
      g.edges.toList match {
        case inner_1 :: inner_2 :: Nil =>
          inner_1 should not equal inner_2
          inner_1 should equal(outer_1)
        case _ => fail()
      }
    }

    def `edge factory shortcuts`: Unit = {
      import Aviation.Implicits._
      import Flight._

      val outer = Flight(ham, gig, flightNo)
      ham ~> gig ## flightNo should be(outer)
      ham ~> gig ## (flightNo, 11 o 20) should be(outer)
    }

    def `pattern matching`: Unit = {
      val g: Graph[Airport, Flight] = factory.empty[Airport, Flight]

      g.nodes foreach { case g.InnerNode(inner, Airport(code)) =>
        code -> inner.outDegree
      }
      g.nodes.outerIterator foreach { case Airport(code) =>
        code
      }
      g.edges foreach { case g.InnerEdge(g.InnerDiEdge(source, _), Flight(_, _, no, _, _)) =>
        no -> source.outDegree
      }
      g.edges.outerIterator foreach { case Flight(from, to, no, _, _) =>
        (from.code, to.code, no)
      }
    }
  }
}
