package demo

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.edges._

import java.time.DayOfWeek._
import java.time.LocalTime

class FlightDemoSpec extends RefSpec with Matchers {

  object `demonstrating how to` {

    def `work with the Flight typed edge`: Unit = {
      import scala.concurrent.duration._

      import scalax.collection.labeled.aviation._
      import scalax.collection.labeled.aviation.immutable._
      import scalax.collection.labeled.aviation.immutable.FlightGraph.OuterImplicits._

      val amsterdam = Airport("AMS")
      val hamburg   = Airport("HAM")
      val newYork   = Airport("JFK")
      val london    = Airport("LHR")
      val mexico    = Airport("MEX")

      val g = FlightGraph(
        hamburg ~> amsterdam :++ ("KL 1776", List(
          MONDAY   -> LocalTime.of(17, 50),
          SATURDAY -> LocalTime.of(17, 40)
        ), 50.minutes),
        hamburg ~> london :++ ("BA 967", List(
          TUESDAY  -> LocalTime.of(8, 20),
          SATURDAY -> LocalTime.of(8, 20)
        ), 1.hour + 10.minutes),
        london ~> newYork :++ ("UA 921", List(
          THURSDAY -> LocalTime.of(18, 0)
        ), 5.hours + 40.minutes),
        newYork ~> mexico :++ ("VB 101", List(
          TUESDAY -> LocalTime.of(14, 10),
          SUNDAY  -> LocalTime.of(14, 20)
        ), 4.hours + 25.minutes)
      )

      /* using the standard extractor of `Flight`
       */
      g.find(hamburg).map { ham =>
        ham.diSuccessors shouldBe Set(amsterdam, london)
        ham.outgoing.map { case g.InnerEdge(_, flight @ Flight(from, to, flightNo, days, duration)) =>
        // TODO flight.toString shouldBe s"Flight($from, $to, $flightNo, $days, $duration)"
        }
      }

      /* using the additional infix extractors `:~>` and `+`
       */
      g.find(hamburg).map { ham =>
        ham.diSuccessors shouldBe Set(amsterdam, london)
        ham.outgoing.map { case g.InnerEdge(_, flight @ from :~> to +: ((flightNo, times, duration))) =>
        // TODO flight.toString shouldBe s"Flight($from, $to, $flightNo, $times, $duration)"
        }
      }
    }
  }
}
