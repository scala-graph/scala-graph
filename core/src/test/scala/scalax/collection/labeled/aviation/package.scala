package scalax.collection.labeled

import scalax.collection.edges.DiEdge

import java.time.{DayOfWeek, LocalTime}
import scala.concurrent.duration._

/** This object encapsulates the node and other helper types needed for
  * the flight route example. The nodes of such a graph will be `Airport`s,
  * the edges `Flight`s using `Datetime` and `Duration`.
  */
package object aviation {

  implicit class FlightConstructorShortcut(val e: DiEdge[Airport]) extends AnyVal {

    def %(flightNo: String, departures: List[(DayOfWeek, LocalTime)], duration: FiniteDuration) =
      Flight(e.source, e.target, flightNo, departures, duration)
  }

  type Labels = (String, List[(DayOfWeek, LocalTime)], FiniteDuration)
  object :~> {
    def unapply(f: Flight): Option[(Airport, (Airport, Labels))] =
      Some(f.departure -> (f.destination, (f.flightNo, f.departures, f.duration)))
  }
  object % {
    def unapply(rest: (Airport, Labels)): Option[(Airport, Labels)] = Some(rest)
  }

}
