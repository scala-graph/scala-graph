package demo

import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.immutable.Graph

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-customizing.html
  *  Customizing Graphs]].
  */
class EnrichingDemoSpec extends RefSpec with Matchers {

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
  }
}
