package demo

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.generic._
import scalax.collection.{immutable, mutable}
import scala.collection.SortedSet

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-operations.html
  *  Graph Operations]].
  */
final class EditingDemoSpec extends RefSpec with Matchers {

  object `demonstrating ` {

    def `Add and Subtract Elements`(): Unit = {
      import immutable.Graph
      val g = Graph(1, 2 ~ 3) // Graph(NodeSet(1, 2, 3), EdgeSet(2 ~ 3))
      g + 1 shouldBe g
      g + 0 shouldBe Graph(0, 1, 2 ~ 3)
      "g + 1.2" shouldNot compile // error: overloaded method...
      g + 0 ~ 1 shouldBe Graph(0, 1, 0 ~ 1, 2 ~ 3)
      g ++ List(1 ~ 2, 2 ~ 3) shouldBe Graph(1 ~ 2, 2 ~ 3)
      g ++ (0 :: Nil, 1 ~ 2 :: 2 ~ 3 :: Nil) shouldBe Graph(0, 1, 1 ~ 2, 2 ~ 3)
      g - 0 shouldBe g
      g - 1 shouldBe Graph(2 ~ 3)
      g - 2 shouldBe Graph(1, 3)
      g - 2 ~ 3 shouldBe Graph(1, 2, 3)
      g -- (2 :: Nil, 3 ~ 3 :: Nil) shouldBe Graph(1, 3)
    }

    def `Add Elements to mutable.Graph`(): Unit = {
      import mutable.Graph
      (Graph(1, 2 ~ 3) += 0) shouldBe Graph(0, 1, 2 ~ 3)
      (Graph[Int, AnyEdge](1, 2 ~ 3) += 3 ~> 1) shouldBe Graph[Int, AnyEdge](1, 2 ~ 3, 3 ~> 1)
    }

    def `equality ` : Unit = {
      val g = immutable.Graph(1 ~ 2)
      g get 1 shouldBe 1
      g get 1 ~ 2 shouldBe 2 ~ 1
      g get 1 ~ 2 eq 2 ~ 1 shouldBe false
      g get 1 ~ 2 should not be 2 ~ 2
    }

    def `union, diff, intersect ` : Unit = {
      import immutable.Graph
      val g = Graph(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
      val h = Graph(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

      g union h shouldBe Graph(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5, 3 ~ 4, 4 ~ 6, 5 ~ 6)
      g diff h shouldBe Graph(1 ~ 2)
      g intersect h shouldBe Graph(4, 3 ~ 5)
      g & h shouldBe Graph(4, 3 ~ 5)
    }

    def `endpoints ` : Unit = {
      val uE = 3 ~ 4 // UnDiEdge[Int]

      uE._1 * uE._2 shouldBe 12
      uE.ends.iterator.product shouldBe 12
      (uE match {
        case n ~ m => n * m
      }) shouldBe 12

      val dE = 1 ~> 2 // DiEdge[Int]
      dE.source - dE.target shouldBe -1
      uE.arity == dE.arity shouldBe true
      (dE match {
        case s ~> t => s - t
      }) shouldBe -1

      import scalax.collection.hyperedges._
      val hE = 1 ~~ 2 ~~ 11 ~~ 12 // HyperEdge[Int]
      hE.node(hE.arity - 1) shouldBe 12
      hE.ends.iterator.sum shouldBe 26
    }

    def `neighbors ` : Unit = {
      import immutable.Graph
      val g = Graph[Int, AnyEdge](0, 1 ~ 3, 3 ~> 2)

      def n(outer: Int): g.NodeT = g get outer

      def e(outer: AnyEdge[Int]): g.EdgeT = g get outer

      n(0).diSuccessors shouldBe Set.empty[g.NodeT]
      n(2).diSuccessors.isEmpty shouldBe true
      n(3).diSuccessors shouldBe Set(n(1), n(2))
      n(3).diPredecessors shouldBe Set(n(1))
      n(2).incoming shouldBe Set(e(3 ~> 2))
      n(3) findOutgoingTo n(2) shouldBe Some(e(3 ~> 2))
    }

    def `querying ` : Unit = {
      import immutable.Graph
      val g = Graph[Int, AnyEdge](2 ~> 3, 3 ~ 1, 5)

      def n(outer: Int): g.NodeT = g get outer

      g.nodes filter (_ > 2) shouldBe Set(n(5), n(3))
      g.nodes filter (_.degree > 1) shouldBe Set(n(3))
      g.edges filter (_ contains 4) shouldBe Symbol("empty")
    }

    def `measuring ` : Unit = {
      import immutable.Graph
      import scalax.collection.edges.labeled._
      val g = Graph[Int, AnyEdge](
        1 ~ 2  % 4,
        2 ~ 3  % 2,
        1 ~> 3 % 5,
        1 ~ 5  % 3,
        3 ~ 5  % 2,
        3 ~ 4  % 1,
        4 ~> 4 % 1,
        4 ~> 5 % 0
      )
      g.order shouldBe 5
      g.size shouldBe 8
      g.totalDegree shouldBe 16
      g.degreeSet shouldBe SortedSet(4, 3, 2)
      g.degreeNodeSeq(g.InDegree) shouldBe List((4, 3), (3, 5), (2, 1), (2, 2), (2, 4))
      g.degreeNodesMap should contain only (2                       -> Set(2), 3 -> Set(5, 1), 4 -> Set(3, 4))
      g.degreeNodesMap(degreeFilter = _ > 3) should contain only (4 -> Set(3, 4))
    }

    def `classifying ` : Unit = {
      import immutable.Graph
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
  }
}
