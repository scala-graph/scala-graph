package custom.flight

import scalax.collection.GraphEdge._

/*
  class A
  abstract class F extends AbstractEdge[A]
  type GenericF[+N] = EdgeLike[N] with F // mimic non-existent type parameter
  Graph.empty[A, GenericF]

  type FlightGraph = Graph[A, GenericF]
  val g: FlightGraph
  g.edges.head.outer
*/
/* TODO
  case class F[+N <: Int](source: Int, target: Int) extends AnyDiEdge[Int] with NodeMapper[Int] {
    validate()
    def map[NN, EE[X] <: AbstractEdge[X]](node_1: NN, node_2: NN): EE[NN] = (node_1, node_2) match {
      case (s: Int, t: Int) => copy(s, t)
      case (s, t)           => DiEdge[NN](s, t)
    }
  }
  Graph.empty[Int, F]
*/

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
  * @param fromAirport The start airport
  * @param toAirport The destination airport
  * @param flightNo The flight Id as a key attribute consisting of the airline short and a flight number of that airline.
  * @param departure daytime of departure
  * @param duration of flight
  */
case class Flight[+N](fromAirport: N,
                      toAirport: N,
                      flightNo: String,
                      departure: DayTime = DayTime(0, 0),
                      duration: Duration = Duration(0, 0))
    extends AbstractGenericDiEdge[N, Flight]
    with ExtendedKey[N] {

  def source: N = fromAirport
  def target: N = toAirport

  def keyAttributes: Seq[String]            = Seq(flightNo)
  override def weight: Double               = duration.toInt
  def airline: String                       = flightNo substring (0, 2)
  def map[NN](n_1: NN, n_2: NN): Flight[NN] = copy(n_1, n_2)

  override protected def attributesToString: String = s" ($flightNo $departure $duration)"
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

    def ##(flightNo: String) = new Flight[A](e.source, e.target, flightNo)

    def ##(flightNo: String, departure: DayTime = DayTime(0, 0), duration: Duration = Duration(0, 0)) =
      new Flight[A](e.source, e.target, flightNo, departure, duration)
  }
}
