package custom.flight

import language.implicitConversions
import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._

/**
 * This object defines the edge type `Flight` needed for the flight route
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
/**
 * Represents a flight between two airports.
 *
 * @param nodes The source and target airports of the flight. 
 * @param flightNo The flight Id as a key attribute consisting of the
 *                 airline short and a flight number of that airline.
 * @param departure daytime of departure
 * @param duration of flight
 */
class Flight[N](nodes: Product,
                val flightNo: String,
                val departure: DayTime = DayTime(0,0),
                val duration: Duration = Duration(0,0))
  extends DiEdge[N](nodes)
  with    ExtendedKey[N]
  with    EdgeCopy[Flight]
  with    EdgeIn[N,Flight] 
{
  def keyAttributes = Seq(flightNo)
  override def weight = duration.toInt
  def airline = flightNo substring (0,2)
  override def copy[NN](newNodes: Product) =
    new Flight[NN](newNodes, flightNo, departure, duration)
  override protected def attributesToString =
    " (" + flightNo + " " + departure + " " + duration + ")" 
}
object Flight {
  def apply(from: Airport,
            to: Airport,
            flightNo:String,
            departure: DayTime = DayTime(0,0),
            duration: Duration = Duration(0,0)) =
    new Flight[Airport](NodeProduct(from, to), flightNo, departure, duration)
  def unapply(e: Flight[Airport]) = Some(e)
}

/**
 * Declares the `Flight` edge factory shortcut `##` which can be invoked like
 * {{{
 * val (hamburg, newYork) = (Airport("HAM"), Airport("JFK"))
 * hamburg ~> newYork ## ("AL 007", 15 o 05, 10 h 20) // yields Flight[Airport]
 * }}}
 */
final class FlightAssoc[A <: Airport](val e: DiEdge[A]) {
  @inline final def ## (flightNo: String) = new Flight[A](e.nodes, flightNo)
  @inline final def ## (flightNo: String,
                        departure: DayTime = DayTime(0,0),
                        duration: Duration = Duration(0,0)) =
    new Flight[A](e.nodes, flightNo, departure, duration)
}
object FlightImplicits {
  /** Enables implicit usage of `FlightAssoc` methods */
  @inline final
  implicit def edge2FlightAssoc[A <: Airport](e: DiEdge[A]) = new FlightAssoc[A](e)
}