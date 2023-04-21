package demo

import scala.concurrent.duration._
import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.generic.AnyEdge
import scalax.collection.immutable.Graph

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

final class TransformingDemoSpec extends RefSpec with Matchers {

  object `demonstrating ` {
    def `Filter your graph`(): Unit = {
      val g = Graph[Int, AnyEdge](2 ~> 3, 3 ~ 1, 5)

      g filter (nodeP = _ >= 2) should ===(Graph(2, 3, 5, 2 ~> 3))
      g filter (edgeP = _.isDirected) should ===(Graph(1, 5, 2, 3, 2 ~> 3))
      g filter (nodeP = _ >= 2, edgeP = _.isUndirected) should ===(Graph(2, 3, 5))
    }

    def `Fold your graph`(): Unit = {
      val g = Graph(1 ~> 2, 1 ~> 3, 4)

      g.foldLeft(Graph.empty[Int, DiEdge[Int]])(
        opNode = {
          case (cum, g.InnerNode(n, _)) if n.isIsolated => cum + n
          case (cum, _)                                 => cum
        },
        opEdge = {
          case (cum, g.InnerEdge(e, _)) if cum.size % 2 == 0 => cum + e
          case (cum, _) => cum
        }
      ) should (be(Graph(4, 1 ~> 3)) or be(Graph(4, 1 ~> 2))) // Graph(NodeSet(1, 2, 4), EdgeSet(1 ~> 2))
    }

    def `Map your generic graph`(): Unit =
      Graph(1 ~ 2).map(
        fNode = _.outer + 1,
        fEdge = (n1: Int, n2: Int) => n1 ~> n2
      ) shouldEqual Graph(2 ~> 3)

    def `Flat-map your generic graph`: Unit =
      Graph(1 ~ 2).flatMap(
        fNode = _.outer + 1 :: Nil,
        fEdge = (n1s: Seq[Int], n2s: Seq[Int]) =>
          (n1s, n2s) match {
            case (Seq(n1, _*), Seq(n2, _*)) => List(n1 ~> n2, n2 ~> n1)
          }
      ) shouldEqual Graph(2 ~> 3, 3 ~> 2)

    def `Map your typed graph`(): Unit = {
      import scalax.collection.labeled.aviation._
      import FlightDemoSpec._

      val hamReplacedByFra =
        g.mapBound((airport: g.NodeT) => if (airport == hamburg) frankfurt else airport)

      hamReplacedByFra should have size g.size
      hamReplacedByFra.nodes should not contain hamburg

      def hamInvolved(flight: Flight): Boolean =
        flight.departure == hamburg ||
          flight.destination == hamburg
      def replaced(flight: Flight) = flight.copy(
        departure = if (flight.departure == hamburg) frankfurt else flight.departure,
        destination = if (flight.destination == hamburg) frankfurt else flight.destination
      )
      g.edges.outerIterable.filter(hamInvolved) foreach { originalFlight =>
        hamReplacedByFra.edges.outerIterable should contain(replaced(originalFlight))
      }

      g.map(
        fNode = (airport: g.NodeT) => airport.outer.toString,
        fEdge = (from: String, to: String) => DiEdge(from, to)
      ) shouldEqual Graph.from(
        g.edges.outerIterable.map(flight => DiEdge(flight.departure.toString, flight.destination.toString))
      )
    }

    def `Flat-map your typed graph`(): Unit = {
      import scalax.collection.labeled.aviation._
      import FlightDemoSpec._

      def ham(flight: Flight): Boolean      = flight.departure == hamburg || flight.destination == hamburg
      def shortHam(flight: Flight): Boolean = ham(flight) && flight.duration < 1.hour

      val shortHamReplacedByFra =
        g.flatMapBound(
          fNode = (airport: g.NodeT) => (if (airport == hamburg) frankfurt else airport.outer) :: Nil,
          fEdge = (edge: g.EdgeT, newDepartures: Seq[Airport], newDestinations: Seq[Airport]) => {
            val outer = edge.outer
            if (shortHam(outer))
              outer.copy(departure = newDepartures.head, destination = newDestinations.head) :: Nil
            else if (ham(outer)) Nil
            else outer :: Nil
          }
        )

      val longHamCount = g.edges.outerIterable.count(f => ham(f) && !shortHam(f))
      shortHamReplacedByFra should have size g.size - longHamCount
      shortHamReplacedByFra.nodes should not contain hamburg

      def replaced(flight: Flight) = flight.copy(
        departure = if (flight.departure == hamburg) frankfurt else flight.departure,
        destination = if (flight.destination == hamburg) frankfurt else flight.destination
      )
      g.edges.outerIterable.filter(shortHam) foreach { originalFlight =>
        shortHamReplacedByFra.edges.outerIterable should contain(replaced(originalFlight))
      }

      g.flatMap(
        fNode = (airport: g.NodeT) => {
          def existing = airport.outer.toString
          if (airport == hamburg) existing :: frankfurt.toString :: Nil
          else existing :: Nil
        },
        fEdge = (edge: g.EdgeT, from: Seq[String], to: Seq[String]) => {
          val outer = edge.outer
          if (shortHam(outer)) {
            def replaced(a: Airport) = (if (a == hamburg) frankfurt else a).toString
            DiEdge(replaced(outer.departure), replaced(outer.destination)) :: Nil
          } else if (ham(outer)) Nil
          else DiEdge(from.head, to.head) :: Nil
        }
      )
    }
  }
}
