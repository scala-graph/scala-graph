package custom.flight

import language.{higherKinds, postfixOps}

import scalax.collection.{Graph, GraphLike}
import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCompanion

import org.scalatest.Spec
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import FlightImplicits._, Helper._

@RunWith(classOf[JUnitRunner])
class TFlightRootTest
	extends Suites( 
			new TFlight[scalax.collection.immutable.Graph](
			                  scalax.collection.immutable.Graph),
			new TFlight[scalax.collection.  mutable.Graph](
			                  scalax.collection.  mutable.Graph)
		)
	with ShouldMatchers
{
}
/**	This class tests the custom edge `Flight` to be run for Graph instances created
 *	by the Graph factory and passed to the constructor. For instance,
 *	this allows the same tests to be run for mutable and immutable Graphs.
 */
class TFlight[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
			(val factory: GraphCompanion[CC])
	extends	Spec
	with	ShouldMatchers
{
  val (ham, gig) = (Airport("HAM"), Airport("GIG"))
  val flightNo = "LH007"
  def test_Flight {
    val outer = Flight(ham, gig, flightNo)
    val g = Graph(outer)
    val e  = g.edges.head
    e.edge.nodes.productElement(0).asInstanceOf[AnyRef].getClass should be (
        g.nodes.head.getClass)
    e.from     should be (ham)
    e.to       should be (gig)
    e.flightNo should be (flightNo)
    e          should be (outer)
    e.##       should be (outer.##)
    val eqFlight = Flight(ham, gig, flightNo, 11 o 2)
    e          should be (eqFlight)
    e.##       should be (eqFlight.##)
    val neFlight = Flight(ham, gig, flightNo + "x", 11 o 2)
    e          should not be (neFlight)
    e.##       should not be (neFlight.##)
  }
  def test_FlightShortcut {
    val outer = Flight(ham, gig, flightNo)
    ham ~> gig ## flightNo            should be (outer)
    ham ~> gig ## (flightNo, 11 o 20) should be (outer)
  }
}