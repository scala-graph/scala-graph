package demo

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OuterImplicits.*
import scalax.collection.edges.*
import scalax.collection.generic.*
import scalax.collection.immutable.Graph

class EnrichingDemoSpec extends RefSpec with Matchers {

  object `demonstrating how to` {

    def `enrich graphs`: Unit = {
      implicit class ExtGraph[N, E <: Edge[N]](protected val g: Graph[N, E]) {
        def foo: String = "bar"
      }
      Graph(1 ~ 2).foo shouldBe "bar"
    }

    def `enrich directed graphs`: Unit = {
      implicit class ExtGraph[N, E <: AnyDiEdge[N]](protected val g: Graph[N, E]) {
        def foo: String = "bar"
      }
      Graph(1 ~> 2).foo shouldBe "bar"
      "Graph(1 ~ 2).foo" shouldNot typeCheck
    }

    def `enrich inner nodes`: Unit = {
      // works for any Graph due to projection type
      implicit class ExtInnerNode[N, E <: Edge[N]](node: Graph[N, E]#NodeT) {
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
