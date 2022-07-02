package scalax.collection.labeled

import scalax.collection.edges.DiEdge
import scalax.collection.generic.{UnapplyLabel, UnapplyLabeledEdge}

import java.time.{DayOfWeek, LocalTime}
import scala.concurrent.duration._

/** Convenience infix constructors and extractors for the `Flight` edge.
  */
package object aviation {

  /** Facilitates infix constructor `airportA ~> airportB + (flightNo, departures, duration)`
    */
  implicit class InfixFlightConstructor(val e: DiEdge[Airport]) extends AnyVal {

    def +(flightNo: String, departures: List[(DayOfWeek, LocalTime)], duration: FiniteDuration) =
      Flight(e.source, e.target, flightNo, departures, duration)
  }

  type Labels = (String, List[(DayOfWeek, LocalTime)], FiniteDuration)

  /** Allows for pattern `airportA :~> airportB + (flightNo, departures, duration)`
    */
  object :~> extends UnapplyLabeledEdge[Airport, Flight, Labels] {
    protected def label(edge: Flight): Labels = (edge.flightNo, edge.departures, edge.duration)
  }

  object + extends UnapplyLabel[Airport, Labels]
}
