package demo

import scala.language.higherKinds

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.Graph

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-customizing.html
  *  Customizing Graphs]].
  */
@RunWith(classOf[JUnitRunner])
class CustomizingTest extends RefSpec with Matchers {

  object `demonstraiting how to` {

    def `enrich graphs`: Unit = {
      implicit class ExtGraph[N, E[X] <: EdgeLikeIn[X]](protected val g: Graph[N, E]) {
        def foo: String = "foo"
      }
      val g           = Graph(1 ~ 2)
      val foo: String = g.foo
    }

    def `enrich inner nodes`: Unit = {
      implicit class ExtGraphNode[N, E[X] <: EdgeLikeIn[X]](node_ : Graph[N, E]#NodeT) {
        protected type NodeT = graph.NodeT
        protected val graph       = node_.containingGraph
        protected val node: NodeT = node_.asInstanceOf[NodeT]

        def foo: String = this.toString + "bar"
      }
      val n = Graph(1 ~ 2).nodes.headOption
      n map (_.foo)
    }

    def `work with a custom edge`: Unit = {
      case class Airport(code: String) {
        override def toString: String = code // without Airport-prefix
      }
      val (ham, ny) = (Airport("HAM"), Airport("JFK"))

      class Flight[N](nodes: Product, val flightNo: String)
          extends DiEdge[N](nodes)
          with ExtendedKey[N]
          with EdgeCopy[Flight]
          with OuterEdge[N, Flight] {

        def keyAttributes = Seq(flightNo)
        override def copy[NN](newNodes: Product) =
          new Flight[NN](newNodes, flightNo)
      }
      object Flight {
        def apply(from: Airport, to: Airport, no: String) =
          new Flight[Airport](NodeProduct(from, to), no)
        def unapply(e: Flight[Airport]): Option[(Airport, Airport, String)] =
          if (e eq null) None else Some(e.from, e.to, e.flightNo)
      }

      implicit class FlightAssoc[A <: Airport](val e: DiEdge[A]) {
        @inline def ##(flightNo: String) = new Flight[A](e.nodes, flightNo) with OuterEdge[A, Flight]
      }

      val flight = ham ~> ny ## "007"
      val g2     = Graph(flight)
    }
  }
}
