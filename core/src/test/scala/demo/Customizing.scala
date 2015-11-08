package demo

import scala.language.higherKinds

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.Graph

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-customizing.html
 *  Customizing Graphs]]. 
 */
@RunWith(classOf[JUnitRunner])
class CustomizingTest
    extends Suite
       with ShouldMatchers {

  import scalax.collection.config.CoreConfig
  import scalax.collection.mutable.ArraySet.Hints
  implicit val myConfig = CoreConfig(orderHint = 5000, Hints(64, 0, 64, 75))
  
  implicit class ExtGraph[N, E[X] <: EdgeLikeIn[X]](protected val g: Graph[N,E]) {
    def foo = "bar"
    def aNode: Option[g.NodeT] = g.nodes.headOption
  }
  val g = Graph(1~2)
  g.foo
  g.aNode
  
  implicit class ExtGraphNode[N, E[X] <: EdgeLikeIn[X]](node_ : Graph[N,E]#NodeT) {
    type NodeT = graph.NodeT
    val graph = node_.containingGraph
    val node  = node_.asInstanceOf[NodeT]
    def foo = this.toString + "bar"
  }
  Graph(1~2).nodes.headOption map (_.foo)
  
  case class Airport(val code: String) {
  override def toString = code // without Airport-prefix
  }
  val (ham, ny) = (Airport("HAM"), Airport("JFK"))
  
  class Flight[N](nodes: Product, val flightNo: String)
      extends DiEdge[N](nodes)
      with    ExtendedKey[N]
      with    EdgeCopy[Flight]
      with    OuterEdge[N,Flight] {

    def keyAttributes = Seq(flightNo)
    override def copy[NN](newNodes: Product) =
      new Flight[NN](newNodes, flightNo)
  }
  object Flight {
    def apply(from: Airport, to: Airport, no:String) =
      new Flight[Airport](NodeProduct(from, to), no)
    def unapply(e: Flight[Airport]): Option[(Airport,Airport,String)] =
        if (e eq null) None else Some(e.from, e.to, e.flightNo)
  }
  implicit class FlightAssoc[A <: Airport](val e: DiEdge[A]) {
    @inline def ## (flightNo: String) =
      new Flight[A](e.nodes, flightNo) with OuterEdge[A,Flight]
  }

  val flight = ham ~> ny ## "007"
  val g2 = Graph(flight)
}