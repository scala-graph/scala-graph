package scalax.collection.labeled.aviation

import scalax.collection.{One, OneOrMore}
import scalax.collection.generic.{AbstractDiEdge, LDiEdgeToString, MultiEdge, MultiLEdgeToString, PartialEdgeMapper}

import java.time.{DayOfWeek, LocalTime}
import scala.concurrent.duration.FiniteDuration

/** A labeled edge to represent flights between airports.
  *
  * Defines a custom edge type that can be used for a flight route map.
  *
  * A `Flight` has several attributes like `departure` and `duration`.
  * To enable multiple flights between airports, the key is extended by `flightNo`.
  *
  * @see EditingTypedSpec.scala.
  *
  * @param departure The departure airport
  * @param destination The destination airport
  * @param flightNo The flight Id as a key attribute consisting of the airline short and a flight number of that airline.
  * @param departures daytime of departure
  * @param duration of flight
  */
case class Flight(
    departure: Airport,
    destination: Airport,
    flightNo: String,
    departures: List[(DayOfWeek, LocalTime)],
    duration: FiniteDuration
) extends AbstractDiEdge[Airport](departure, destination)
    with MultiEdge
    with LDiEdgeToString
    with MultiLEdgeToString
    with PartialEdgeMapper[Flight] {

  override def weight: Double = duration.toMinutes.toDouble

  def airline: String = flightNo.takeWhile(_.isLetter)

  override def extendKeyBy: OneOrMore[String] = One(flightNo)

  override protected def labelToString: String = s"($flightNo, $departures, $duration)"

  override def map[N]: PartialFunction[(N, N), Flight] = { case (from: Airport, to: Airport) =>
    copy(from, to)
  }
}
