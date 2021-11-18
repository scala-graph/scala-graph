package scalax.collection

import org.scalatest._
import org.scalatest.refspec.RefSpec

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.generic.GraphCoreCompanion
import org.scalatest.matchers.should.Matchers

class MappingSpec
    extends Suites(
      new Mapping[immutable.Graph](immutable.Graph),
      new Mapping[mutable.Graph](mutable.Graph)
    )

private class Mapping[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC]
) extends RefSpec
    with Matchers {

  object `the map of an undirected graph with generic edges` {
    private val edge      = 1 ~ 2
    private val originalG = factory(edge)

    private def fNode(n: originalG.NodeT) = n.outer + 1

    def `yields another graph` {
      val g = originalG map fNode

      g shouldBe a[CC[Int, UnDiEdge[Int]] @unchecked]
      g.nodes.head.outer shouldBe an[Integer]
      g.edges.head shouldBe an[g.Inner.UnDiEdge]
      (g.edges.head.outer: UnDiEdge[Int]) shouldBe an[UnDiEdge[_]]
    }
    def `has correctly mapped nodes` {
      val g = originalG map fNode

      originalG.nodes zip g.nodes.toOuter foreach {
        case (original, mapped) => fNode(original) == mapped
      }
    }
    def `has correctly mapped edges` {
      val g = originalG map fNode

      g.edges.head should be(UnDiEdge(2, 3))
    }
    def `may have a new node type` {
      val g = originalG map (_.toString)

      g.nodes.head.outer shouldBe a[String]
      (g.edges.head.outer: UnDiEdge[String]) shouldBe an[UnDiEdge[_]]
      g.edges.head should be(edge._1.toString ~ edge._2.toString)
    }
    def `may yield a directed graph`: Unit = {
      val g = originalG map ({ case originalG.InnerNode(i) => i + 1 }, DiEdge[Int])
      (g: CC[Int, DiEdge[Int]]).edges.head.isDirected shouldBe true
    }
  }
}
