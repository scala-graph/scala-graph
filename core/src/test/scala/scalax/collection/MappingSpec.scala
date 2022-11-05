package scalax.collection

import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites
import org.scalatest.refspec.RefSpec

import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.OuterImplicits._
import scalax.collection.generic.GenericGraphCoreFactory

import MappingSpec._

class MappingSpec
    extends Suites(
      new Mapping[immutable.Graph](immutable.Graph),
      new Mapping[mutable.Graph](mutable.Graph)
    )

object MappingSpec {
  private class Mapping[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
      val factory: GenericGraphCoreFactory[CC]
  ) extends RefSpec
      with Matchers {

    object `mapping a generic undirected graph you can` {
      private val edge      = 1 ~ 2
      private val originalG = factory(edge)

      private def increment(n: originalG.NodeT) = n.outer + 1

      def `create another graph`: Unit = {
        val g = originalG map increment

        g shouldBe a[CC[Int, UnDiEdge[Int]] @unchecked]
        g.nodes.head.outer shouldBe an[Integer]
        g.edges.head shouldBe an[g.InnerUnDiEdge]
        (g.edges.head.outer: UnDiEdge[Int]) shouldBe an[UnDiEdge[_]]
      }

      def `map by nodes`: Unit = {
        val g = originalG map increment

        originalG.nodes zip g.nodes.outerIterator foreach { case (original, mapped) =>
          increment(original) shouldBe mapped
        }
        g.edges.head shouldBe UnDiEdge(2, 3)
      }

      def `change the node type`: Unit = {
        val g = originalG map (_.toString)

        g.nodes.head.outer shouldBe a[String]
        (g.edges.head.outer: UnDiEdge[String]) shouldBe an[UnDiEdge[_]]
        g.edges.head.outer shouldBe (edge._1.toString ~ edge._2.toString)
      }

      def `change the edge type`: Unit = {
        val g = originalG.map(increment, (n1: Int, n2: Int) => n1 ~> n2)
        (g: CC[Int, DiEdge[Int]]) shouldEqual factory((edge._1 + 1) ~> (edge._2 + 1))
        g.isDirected shouldBe true
      }

      def `inspect the edges to be mapped`: Unit = {
        val g =
          originalG.map(
            increment,
            (e: originalG.EdgeT, n1: Int, _: Int) => n1 ~> (e.weight.toInt + 2)
          )
        (g: CC[Int, DiEdge[Int]]) shouldEqual factory(2 ~> 3)
      }

      def `change edge ends`: Unit = {
        val g = originalG.map(_.outer + 1, (n1: Int, _: Int) => n1 ~> 7)
        (g: CC[Int, DiEdge[Int]]) shouldEqual factory(3, 2 ~> 7)
      }
    }

    object `flat-mapping a generic undirected graph you can` {
      private val edge      = 1 ~ 2
      private val originalG = factory(edge)

      def `flat-map by incrementing node values`: Unit = {
        def increment(n: originalG.NodeT): List[Int] = n.outer + 1 :: Nil

        val g = originalG flatMap increment

        originalG.nodes zip g.nodes.outerIterator foreach { case (original, mapped) =>
          increment(original).head shouldBe mapped
        }
        g.edges.head shouldBe UnDiEdge(2, 3)
      }

      def `flat-map nodes`: Unit = {
        val g = originalG flatMap (n => List(n.outer, -n.outer))

        g shouldBe factory(edge, -edge._1, -edge._2)
      }

      def `change the node type`: Unit = {
        val g = originalG flatMap (n => List(n.outer.toString, (-n.outer).toString))

        g shouldBe factory(edge._1.toString ~ edge._2.toString, (-edge._1).toString, (-edge._2).toString)
      }
    }
  }
}
