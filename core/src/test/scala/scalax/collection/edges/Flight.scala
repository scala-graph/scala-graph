package scalax.collection.edges

import scalax.collection.GraphEdge._
import scalax.collection.edges.Aviation._

/** Represents a flight between two airports.
  *
  * This object defines the edge type `Flight` needed for the flight route map example as a custom edge type.
  *
  * A `Flight` has several attributes like `departure` and `duration`.
  * To enable duplicate flights between airports, `flightNo` is added to the key attributes.
  *
  * Note that this is not a real-life design. For instance, calculating the shortest path does not consider
  * waiting times on the airports, it just uses the flight duration.
  *
  * For a usage example see TFlight.scala.
  *
  * @param departure The departure airport
  * @param destination The destination airport
  * @param flightNo The flight Id as a key attribute consisting of the airline short and a flight number of that airline.
  * @param departureAt daytime of departure
  * @param duration of flight
  */
case class Flight(departure: Airport,
                  destination: Airport,
                  flightNo: String,
                  departureAt: DayTime = DayTime(0, 0),
                  duration: Duration = Duration(0, 0))
    extends AbstractDiEdge[Airport](departure, destination)
    with PartialEdgeMapper[Airport, Flight]
    with ExtendedKey[Airport] {

  def keyAttributes: Seq[String] = Seq(flightNo)
  override def weight: Double    = duration.toInt
  def airline: String            = flightNo substring (0, 2)

  override protected def attributesToString: String = s" ($flightNo $departure $duration)"
  override def map[NN]: PartialFunction[(NN, NN), Flight] = {
    case (from: Airport, to: Airport) => copy(from, to)
  }
}

/** It is not necessary to declare this edge companion object unless you want to define your own factory shortcut.
  */
object Flight {

  /** Defines the `Flight` factory shortcut `##` which can be invoked like
    * {{{
    * val (hamburg, newYork) = (Airport("HAM"), Airport("JFK"))
    * hamburg ~> newYork ## ("AL 007", 15 o 05, 10 h 20) // yields Flight[Airport]
    * }}}
    */
  implicit final class ImplicitEdge[A <: Airport](val e: DiEdge[A]) extends AnyVal {

    def ##(flightNo: String) = new Flight(e.source, e.target, flightNo)

    def ##(flightNo: String, departure: DayTime = DayTime(0, 0), duration: Duration = Duration(0, 0)) =
      new Flight(e.source, e.target, flightNo, departure, duration)
  }
}
