package custom.flight

import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/** This object defines the edge type `Flight` needed for the flight route
  * map example as a custom edge type.
  *
  * A `Flight` has several attributes like `departure` and `duration`.
  * To enable duplicate flights between airports, `flightNo` is added
  * to the key attributes.
  *
  * Note that this is not a real-life design. For instance, calculating
  * the shortest path does not consider waiting times on the airports,
  * it just uses the flight duration.
  *
  * For usage see Ttest_shortestPathFunctional in TAlgo.scala.
  * There, things are more complex due to the `factory` variable for
  * which you could just write `Graph` in a real app, instead.
  */
/* ------------------------------------------ custom edge type */
/** Represents a flight between two airports.
  *
  * @param nodes The source and target airports of the flight.
  * @param flightNo The flight Id as a key attribute consisting of the
  *                 airline short and a flight number of that airline.
  * @param departure daytime of departure
  * @param duration of flight
  */
case class Flight[+N](fromAirport: N,
                      toAirport: N,
                      flightNo: String,
                      departure: DayTime = DayTime(0, 0),
                      duration: Duration = Duration(0, 0))
    extends DiEdge[N](NodeProduct(fromAirport, toAirport))
    with ExtendedKey[N]
    with EdgeCopy[Flight]
    with OuterEdge[N, Flight] {

  private def this(nodes: Product, flightNo: String, departure: DayTime, duration: Duration) {
    this(
      nodes.productElement(0).asInstanceOf[N],
      nodes.productElement(1).asInstanceOf[N],
      flightNo,
      departure,
      duration)
  }

  def keyAttributes                         = Seq(flightNo)
  override def weight                       = duration.toInt
  def airline                               = flightNo substring (0, 2)
  override def copy[NN](newNodes: Product)  = new Flight[NN](newNodes, flightNo, departure, duration)
  override protected def attributesToString = s" ($flightNo $departure $duration)"
}

object Flight {

  /** Declares the `Flight` edge factory shortcut `##` which can be invoked like
    * {{{
    * val (hamburg, newYork) = (Airport("HAM"), Airport("JFK"))
    * hamburg ~> newYork ## ("AL 007", 15 o 05, 10 h 20) // yields Flight[Airport]
    * }}}
    */
  implicit final class ImplicitEdge[A <: Airport](val e: DiEdge[A]) extends AnyVal {
    def ##(flightNo: String) = new Flight[A](e.source, e.target, flightNo)
    def ##(flightNo: String, departure: DayTime = DayTime(0, 0), duration: Duration = Duration(0, 0)) =
      new Flight[A](e.nodes, flightNo, departure, duration)
  }
}
