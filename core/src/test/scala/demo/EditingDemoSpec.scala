package demo

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.Graph
import scalax.collection.immutable
import scalax.collection.mutable

import org.scalatest.refspec.RefSpec
import org.scalatest.matchers.should.Matchers


/** Includes the examples given on [[http://www.scala-graph.org/guides/core-operations.html
  *  Graph Operations]].
  */
final class EditingDemoSpec extends RefSpec with Matchers {

  object `demonstraiting ` {

    def `iterating ` : Unit = {
      val g = Graph(2 ~ 3, 3 ~ 1)
      g.iterator mkString "," shouldBe "1,2,3,2 ~ 3,3 ~ 1"
      g.nodes mkString "-" shouldBe "1-2-3"
      g.edges mkString " " shouldBe "2 ~ 3 3 ~ 1"
    }

    def `looking up`: Unit = {
      val g = Graph(1 ~ 2)
      g find 1 shouldBe Some(1) // Option[g.NodeT]
      g find 3 shouldBe None    // Option[g.NodeT]
      g get 1 shouldBe 1        // g.NodeT = 1
      a[NoSuchElementException] should be thrownBy {
        g get 3
      }
      g find 1 ~ 2 shouldBe Some(1 ~ 2) // Option[g.EdgeT]
      g find 1 shouldBe Some(1)         // Option[Param[Int,UnDiEdge]]

      val h = mutable.Graph.empty[Int, UnDiEdge[Int]] |= g
      h addAndGet 5 shouldBe 5 // g.NodeT
    }

    def `equality ` : Unit = {
      val g = Graph(1 ~ 2)
      (g get 1) shouldBe 1
      (g get 1 ~ 2) shouldBe 2 ~ 1
      (g get 1 ~ 2) eq 2 ~ 1 shouldBe false
      (g get 1 ~ 2) should not be 2 ~ 2
    }

    def `adding ` : Unit = {
      val g = immutable.Graph(1, 2 ~ 3)
      g + 1 shouldBe g
      g + 0 shouldBe Graph(0, 1, 2, 3, 2 ~ 3)
      g + 0 ~ 1 shouldBe Graph(0, 1, 2, 3, 0 ~ 1, 2 ~ 3)
      g ++ (edges = List(1 ~ 2, 2 ~ 3)) shouldBe Graph(1, 2, 3, 1 ~ 2, 2 ~ 3)
      g ++ (edges = List(1 ~ 2, 2 ~ 3), isolatedNodes = List(0)) shouldBe Graph(0, 1, 2, 3, 1 ~ 2, 2 ~ 3)
      g - 0 shouldBe g
      g - 1 shouldBe Graph(2, 3, 2 ~ 3)
      g - 2 shouldBe Graph(1, 3)
      g - 2 ~ 3 shouldBe Graph(1, 2, 3)
      g -- (List(2), List(3 ~ 3)) shouldBe Graph(1, 3)

      def h = mutable.Graph.from[Int, AnyEdge[Int]](nodes = g.nodes.toOuter, edges = g.edges.toOuter)
      (h addOne 0) shouldBe Graph(0, 1, 2, 3, 2 ~ 3)
      (h += 3 ~> 1) shouldBe Graph[Int, AnyEdge](1, 2, 3, 2 ~ 3, 3 ~> 1)
    }

    def `union, diff, intersect ` : Unit = {
      val g = immutable.Graph(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
      val h = immutable.Graph(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

      g union h shouldBe Graph(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5, 3 ~ 4, 4 ~ 6, 5 ~ 6)
      g diff h shouldBe Graph(1 ~ 2)
      g intersect h shouldBe Graph(4, 3 ~ 5)
      (g & h) shouldBe Graph(4, 3 ~ 5)
    }

    def `endpoints ` : Unit = {
      val uE = 3 ~ 4 // UnDiEdge[Int]

      uE._1 * uE._2 shouldBe 12
      uE.ends.product shouldBe 12
      (uE match {
        case n ~ m => n * m
      }) shouldBe 12

      val dE = 1 ~> 2 // DiEdge[Int]
      dE.source - dE.target shouldBe -1
      uE.arity == dE.arity shouldBe true
      (dE match {
        case s ~> t => s - t
      }) shouldBe -1

      import scalax.collection.GraphPredef.HyperEdgeImplicits._
      val hE = 1 ~~ 2 ~~ 11 ~~ 12 // HyperEdge[Int]
      hE._n(hE.arity - 1) shouldBe 12
      hE.ends.sum shouldBe 26
    }

    /* TODO
    def `edge patterns`: Unit = {
      import scalax.collection.edge.Implicits._
      val g = Graph((1 ~+> 2)("A"), (1 ~+> 1)("AB"))

      import scalax.collection.edge.LBase._
      object StringLabel extends LEdgeImplicits[String]

      (0 /: g.edges)((sum, e) =>
        e.edge match {
          case s :~> t + (l: String) if l contains 'A' =>
            sum + s.outDegree + t.outDegree
      }) shouldBe 6
    }
     */

    def `neighbors ` : Unit = {
      val g                               = Graph[Int, AnyEdge](0, 1 ~ 3, 3 ~> 2)
      def n(outer: Int): g.NodeT          = g get outer
      def e(outer: AnyEdge[Int]): g.EdgeT = g get outer

      n(0).diSuccessors shouldBe Set.empty[g.NodeT]
      n(2).diSuccessors.isEmpty shouldBe true
      n(3).diSuccessors shouldBe Set(n(1), n(2))
      n(3).diPredecessors shouldBe Set(n(1))
      n(2).incoming shouldBe Set(e(3 ~> 2))
      (n(3) ~>? n(2)) shouldBe Some(e(3 ~> 2))
    }

    def `querying ` : Unit = {
      val g                      = Graph[Int, AnyEdge](2 ~> 3, 3 ~ 1, 5)
      def n(outer: Int): g.NodeT = g get outer

      g.nodes filter (_ > 2) shouldBe Set(n(5), n(3))
      g.nodes filter (_.degree > 1) shouldBe Set(n(3))
      g.edges filter (_ contains 4) shouldBe Symbol("empty")

      g filter (fNode = _ >= 2) should ===(Graph(2, 3, 5, 2 ~> 3))
      g filter (fEdge = _.isDirected) should ===(Graph(1, 5, 2, 3, 2 ~> 3))
    }

    /* TODO
    def `measuring ` : Unit = {
      import scalax.collection.edge.Implicits._
      val g = Graph(1 ~ 2 % 4, 2 ~ 3 % 2, 1 ~> 3 % 5, 1 ~ 5 % 3, 3 ~ 5 % 2, 3 ~ 4 % 1, 4 ~> 4 % 1, 4 ~> 5 % 0)
      g.order shouldBe 5
      g.graphSize shouldBe 8
      g.size shouldBe 13
      g.totalDegree shouldBe 16
      g.degreeSet shouldBe SortedSet(4, 3, 2)
      g.degreeNodeSeq(g.InDegree) shouldBe List((4, 3), (3, 5), (2, 1), (2, 2), (2, 4))
      g.degreeNodesMap should contain only (2                       -> Set(2), 3 -> Set(5, 1), 4 -> Set(3, 4))
      g.degreeNodesMap(degreeFilter = _ > 3) should contain only (4 -> Set(3, 4))
    }
    def `classifying ` : Unit = {
      val g = Graph(1, 2 ~> 3)
      g.isConnected shouldBe false
      (g + 2 ~> 1).isConnected shouldBe true
      (g get 2).findConnected(_ == 3) shouldBe Some(3)
      g.isCyclic shouldBe false
      (g + 3 ~> 2).isCyclic shouldBe true
      g.isComplete shouldBe false
      (g ++ List(1 ~> 2, 1 ~> 3, 2 ~> 1, 3 ~> 1, 3 ~> 2)).isComplete shouldBe true
      g.isDirected shouldBe true
      g.isHyper shouldBe false
      g.isMulti shouldBe false
    }
   */
  }
}
