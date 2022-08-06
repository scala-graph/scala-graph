package demo

import java.time.DayOfWeek._
import java.time.LocalTime

import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.immutable.Graph

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-customizing.html
  *  Customizing Graphs]].
  */
class CustomizingDemoSpec extends RefSpec with Matchers {

  object `demonstrating how to` {

    def `enrich graphs`: Unit = {
      implicit class ExtGraph[N, E <: Edge[N]](protected val g: Graph[N, E]) {
        def foo: String = "bar"
      }
      val g = Graph(1 ~ 2)
      g.foo shouldBe "bar"
    }

    def `enrich inner nodes`: Unit = {
      // works for any Graph due to projection type
      implicit class ExtGraphNode[N, E <: Edge[N]](node: Graph[N, E]#NodeT) {
        def outOverInDegree: Int = node.outDegree - node.inDegree
      }

      Graph(1 ~> 2).nodes foreach {
        case n if n.outer == 1 => n.outOverInDegree shouldBe 1
        case n if n.outer == 2 => n.outOverInDegree shouldBe -1
        case _                 => fail()
      }
    }

    def `work with a custom edge`: Unit = {
      import scala.concurrent.duration._
      import scalax.collection.labeled.aviation._

      val amsterdam = Airport("AMS")
      val hamburg   = Airport("HAM")
      val newYork   = Airport("JFK")
      val london    = Airport("LHR")
      val mexico    = Airport("MEX")

      val g = Graph(
        hamburg ~> amsterdam + ("KL 1776", List(
          MONDAY   -> LocalTime.of(17, 50),
          SATURDAY -> LocalTime.of(17, 40)
        ), 50.minutes),
        hamburg ~> london + ("BA 967", List(
          TUESDAY  -> LocalTime.of(8, 20),
          SATURDAY -> LocalTime.of(8, 20)
        ), 1.hour + 10.minutes),
        london ~> newYork + ("UA 921", List(
          THURSDAY -> LocalTime.of(18, 0)
        ), 5.hours + 40.minutes),
        newYork ~> mexico + ("VB 101", List(
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
        ham.outgoing.map { case g.InnerEdge(_, flight @ from :~> to + ((flightNo, times, duration))) =>
        // TODO flight.toString shouldBe s"Flight($from, $to, $flightNo, $times, $duration)"
        }
      }
    }
  }
}
