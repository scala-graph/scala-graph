package scalax.collection

import java.time.DayOfWeek._
import java.time.LocalTime
import scala.concurrent.duration._
import org.scalatest.matchers.should.Matchers
import org.scalatest.Suites
import org.scalatest.refspec.RefSpec

import scalax.collection.edges.DiEdge
import scalax.collection.generic._
import scalax.collection.generic.GenericGraphCoreFactory
import scalax.collection.labeled.aviation._

class EditingTypedSpec
    extends Suites(
      new EditingTypedEdges,
      new EditingTyped[scalax.collection.immutable.Graph](scalax.collection.immutable.Graph),
      new EditingTyped[scalax.collection.mutable.Graph](scalax.collection.mutable.Graph)
    )

private object Samples {
  val (madrid, rio) = (Airport("MAD"), Airport("GIG"))
  val flightNo      = "IB 8711"
  val outer = Flight(
    madrid,
    rio,
    flightNo,
    List(
      TUESDAY  -> LocalTime.of(8, 20),
      SATURDAY -> LocalTime.of(8, 20)
    ),
    12.hour + 30.minutes
  )
}

private class EditingTypedEdges extends RefSpec with Matchers {
  import Samples._

  def `toString of typed edge`: Unit =
    outer.toString should startWith(s"$madrid ~> $rio ++ ")
}

private class EditingTyped[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers {
  import Samples._

  object `Custom edge 'Flight'` {
    def `edge methods`: Unit = {

      // TODO get apply/from work with Flight
      val g: AnyGraph[Airport, Flight] =
        factory.from[Airport, Flight](Nil, edges = outer :: Nil) // `annotated for IntelliJ

      val e = g.edges.head
      e.ends.head.getClass should be(g.nodes.head.getClass)
      e.departure shouldBe madrid
      e.destination shouldBe rio
      e.flightNo shouldBe flightNo
      e shouldBe outer
      e.## should be(outer.##)

      val eqFlight = Flight(madrid, rio, flightNo, Nil, outer.duration + 1.minute)
      e shouldBe eqFlight
      e.## should be(eqFlight.##)

      val neFlight = Flight(madrid, rio, flightNo + "x", outer.departures, outer.duration)
      e should not be neFlight
      e.## should not be neFlight.##
    }

    def `edge equality`: Unit = {
      val outer_1 = Flight(madrid, rio, flightNo, outer.departures, outer.duration + 1.minute)
      val outer_2 = Flight(madrid, rio, flightNo + "x", outer.departures, outer.duration + 1.minute)

      outer_1 should not equal outer_2

      val g = factory.from(outer_1 :: outer_2 :: Nil)
      g.edges.toList match {
        case inner_1 :: inner_2 :: Nil =>
          inner_1 should not equal inner_2
          inner_1 should equal(outer_1)
        case _ => fail()
      }

      outer shouldNot equal(DiEdge(madrid, rio))
      DiEdge(madrid, rio) shouldNot equal(outer)
    }

    def `infix constructor`: Unit = {
      import scalax.collection.edges.DiEdgeImplicits
      madrid ~> rio + (flightNo, outer.departures, outer.duration) shouldBe outer
    }

    def `extractor ` : Unit = {
      val g: AnyGraph[Airport, Flight] = factory.empty[Airport, Flight]

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

    def `concat adding a generic edge`: Unit = {
      val g: AnyGraph[Airport, Flight] = factory.empty

      g ++ List(outer) shouldBe factory.from(outer :: Nil)
      "g ++ List(outer): AnyGraph[Airport, Flight]" should compile

      val diEdge  = DiEdge(outer._1, outer._2)
      val widened = g ++ List(diEdge)
      "widened: AnyGraph[Airport, Flight]" shouldNot compile
      "widened: AnyGraph[Airport, DiEdge[Airport]]" shouldNot compile
      "widened: AnyGraph[Airport, AnyDiEdge[Airport] with EdgeMapper with DiEdgeToString with Product with Serializable]" should compile
    }

    def `concat adding a typed edge`: Unit = {
      val g: AnyGraph[Airport, DiEdge[Airport]] = factory.empty
      val diEdge                                = DiEdge(outer._1, outer._2)

      g ++ List(diEdge) shouldBe factory.from(diEdge :: Nil)
      "g ++ List(diEdge): AnyGraph[Airport, DiEdge[Airport]]" should compile

      val widened = g ++ List(outer)
      "widened: AnyGraph[Airport, Flight]" shouldNot compile
      "widened: AnyGraph[Airport, DiEdge[Airport]]" shouldNot compile
      "widened: AnyGraph[Airport, AnyDiEdge[Airport] with EdgeMapper with DiEdgeToString with Product with Serializable]" should compile
    }
  }
}
