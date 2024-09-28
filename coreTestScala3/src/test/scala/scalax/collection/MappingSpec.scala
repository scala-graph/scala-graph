package scalax.collection

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.OuterImplicits.*
import scalax.collection.edges.*
import scalax.collection.immutable.Graph

class MappingSpec extends RefSpec with Matchers {

  object `mapping a generic undirected graph you can` {
    private val edge      = 1 ~ 2
    private val originalG = Graph(edge)

    private def increment(n: originalG.NodeT) = n.outer + 1

    def `create another graph`: Unit = {
      val g = originalG map increment

      g shouldBe a[Graph[Int, UnDiEdge[Int]] @unchecked]
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
      g.edges.head.outer shouldBe (edge.node1.toString ~ edge.node2.toString)
    }

    def `change the edge type`: Unit = {
      val g = originalG.map(increment, (n1: Int, n2: Int) => n1 ~> n2)

      (g: Graph[Int, DiEdge[Int]]) shouldEqual Graph((edge.node1 + 1) ~> (edge.node2 + 1))
      g.isDirected shouldBe true
    }

    def `inspect the edges to be mapped`: Unit = {
      val g =
        originalG.map(
          increment,
          (e: originalG.EdgeT, n1: Int, _: Int) => n1 ~> (e.weight.toInt + 2)
        )
      (g: Graph[Int, DiEdge[Int]]) shouldEqual Graph(2 ~> 3)
    }

    def `change edge ends`: Unit = {
      val g = originalG.map(_.outer + 1, (n1: Int, _: Int) => n1 ~> 7)
      (g: Graph[Int, DiEdge[Int]]) shouldEqual Graph(3, 2 ~> 7)
    }
  }

  object `flat-mapping a generic undirected graph you can` {
    private val edge      = 1 ~ 2
    private val originalG = Graph(edge)

    def increment(n: Graph[Int, UnDiEdge[Int]]#NodeT): List[Int] = n.outer + 1 :: Nil

    def `change nodes`: Unit = {
      val g = originalG flatMap increment

      originalG.nodes zip g.nodes.outerIterator foreach { case (original, mapped) =>
        increment(original).head shouldBe mapped
      }
      g.edges.head shouldBe UnDiEdge(2, 3)
    }

    def `add nodes`: Unit = {
      val g = originalG flatMap (n => List(n.outer, -n.outer))
      g shouldBe Graph(edge, -edge.node1, -edge.node2)
    }

    def `change the node type`: Unit = {
      val g = originalG flatMap (n => List(n.outer.toString, -n.outer.toString))
      g shouldBe Graph(edge.node1.toString ~ edge.node2.toString, -edge.node1.toString, -edge.node2.toString)
    }

    def `change the edge type`: Unit = {
      val g = originalG.flatMap(
        fNode = increment,
        fEdge = (n1s: Seq[Int], n2s: Seq[Int]) =>
          (n1s, n2s) match {
            case (Seq(n1, _*), Seq(n2, _*)) => List(n1 ~> n2, n2 ~> n1)
          }
      )
      (g: Graph[Int, DiEdge[Int]]) shouldEqual Graph(2 ~> 3, 3 ~> 2)
      g.isDirected shouldBe true
    }

    def `change edge ends and structure`: Unit = {
      val source = Graph(1 ~ 2, 3 ~ 3)
      val g: Graph[Int, DiEdge[Int]] =
        source.flatMap(
          fNode = increment,
          fEdge = (e: source.EdgeT, n1s: Seq[Int], n2s: Seq[Int]) =>
            if (e.isLooping) Nil
            else
              (n1s, n2s) match {
                case (Seq(n1, _*), Seq(n2, _*)) =>
                  List(
                    n1 ~> (e.weight.toInt + 10),
                    n2 ~> (e.weight.toInt + 10)
                  )
              }
        )
      g.nodes.outerIterable should contain theSameElementsAs List(2, 3, 4, 11)
      g.edges.outerIterable should contain theSameElementsAs List(2 ~> 11, 3 ~> 11)
    }
  }
}
