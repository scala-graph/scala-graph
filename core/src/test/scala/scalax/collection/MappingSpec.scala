package scalax.collection

import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites
import org.scalatest.refspec.RefSpec

import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.OuterImplicits._
import scalax.collection.generic.GenericGraphCoreFactory

class MappingSpec
    extends Suites(
      new Mapping[immutable.Graph](immutable.Graph),
      new Mapping[mutable.Graph](mutable.Graph)
    )

private class Mapping[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends RefSpec
    with Matchers {

  object `undirected graph with generic edges` {
    private val edge      = 1 ~ 2
    private val originalG = factory(edge)

    private def fNode(n: originalG.NodeT) = n.outer + 1

    def `yields another graph`: Unit = {
      val g = originalG map fNode

      g shouldBe a[CC[Int, UnDiEdge[Int]] @unchecked]
      g.nodes.head.outer shouldBe an[Integer]
      g.edges.head shouldBe an[g.InnerUnDiEdge]
      (g.edges.head.outer: UnDiEdge[Int]) shouldBe an[UnDiEdge[_]]
    }

    def `map by nodes`: Unit = {
      val g = originalG map fNode

      originalG.nodes zip g.nodes.outerIterator foreach { case (original, mapped) =>
        fNode(original) == mapped
      }
      g.edges.head should be(UnDiEdge(2, 3))
    }

    def `change node type`: Unit = {
      val g = originalG map (_.toString)

      g.nodes.head.outer shouldBe a[String]
      (g.edges.head.outer: UnDiEdge[String]) shouldBe an[UnDiEdge[_]]
      g.edges.head shouldBe (edge._1.toString ~ edge._2.toString)
    }

    def `change edge type`: Unit = {
      val g = originalG.map(_.outer + 1, (n1: Int, n2: Int) => n1 ~> n2)
      (g: CC[Int, DiEdge[Int]]).edges.head.isDirected shouldBe true
    }

    def `inspect edges to map`: Unit = {
      val g =
        originalG.map(
          fNode _,
          (e: originalG.EdgeT, n1: Int, _: Int) => n1 ~> (e.weight.toInt + 2)
        )
      (g: CC[Int, DiEdge[Int]]) shouldEqual factory(2 ~> 3)
    }

    def `change edge ends by permitting implicit node addition`: Unit = {
      val g = originalG.map(_.outer + 1, (n1: Int, _: Int) => n1 ~> 7)
      (g: CC[Int, DiEdge[Int]]) shouldEqual factory(3, 2 ~> 7)
    }
  }
}
