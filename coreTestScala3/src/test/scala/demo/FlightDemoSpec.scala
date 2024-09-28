package demo

import java.time.DayOfWeek.*
import java.time.{DayOfWeek, LocalTime}

import scala.concurrent.duration.*

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.edges.*

class FlightDemoSpec extends RefSpec with Matchers {

  object `demonstrating how to` {

    def `work with the Flight typed edge`: Unit = {
      import FlightDemoSpec.*
      import scalax.collection.labeled.aviation.*

      def expectedStringRepresentation(
          from: Airport,
          to: Airport,
          flightNo: String,
          times: List[(DayOfWeek, LocalTime)],
          duration: FiniteDuration
      ) =
        s"$from ~> $to :++ ($flightNo, $times, $duration)"

      /* look up a node and examine outgoing edges by using the prefix extractor
       */
      g.find(hamburg).map { ham =>
        ham.diSuccessors shouldBe Set(amsterdam, london)
        ham.outgoing.map { case g.InnerEdge(_, flight @ Flight(from, to, flightNo, times, duration)) =>
          flight.toString shouldBe expectedStringRepresentation(from, to, flightNo, times, duration)
        }
      }

      /* same but using the infix extractors `:~>` and `++:` that were added optionally
       */
      g.find(hamburg).map { ham =>
        ham.diSuccessors shouldBe Set(amsterdam, london)
        ham.outgoing.map { case g.InnerEdge(_, flight @ from :~> to ++: ((flightNo, times, duration))) =>
          flight.toString shouldBe expectedStringRepresentation(from, to, flightNo, times, duration)
        }
      }
    }
  }
}

object FlightDemoSpec {
  import scalax.collection.labeled.aviation.*
  import scalax.collection.labeled.aviation.immutable.*
  import scalax.collection.labeled.aviation.immutable.FlightGraph.OuterImplicits.*

  val amsterdam = Airport("AMS")
  val hamburg   = Airport("HAM")
  val frankfurt = Airport("FRA")
  val newYork   = Airport("JFK")
  val london    = Airport("LHR")
  val mexico    = Airport("MEX")

  /* construct the Graph by supplying edges
   */
  val g = FlightGraph(
    hamburg ~> amsterdam :++ (
      "KL 1776",
      List(
        MONDAY   -> LocalTime.of(17, 50),
        SATURDAY -> LocalTime.of(17, 40)
      ),
      50.minutes
    ),
    hamburg ~> london :++ (
      "BA 967",
      List(
        TUESDAY  -> LocalTime.of(8, 20),
        SATURDAY -> LocalTime.of(8, 20)
      ),
      1.hour + 10.minutes
    ),
    london ~> newYork :++ (
      "UA 921",
      List(
        THURSDAY -> LocalTime.of(18, 0)
      ),
      5.hours + 40.minutes
    ),
    newYork ~> mexico :++ (
      "VB 101",
      List(
        TUESDAY -> LocalTime.of(14, 10),
        SUNDAY  -> LocalTime.of(14, 20)
      ),
      4.hours + 25.minutes
    )
  )
}
