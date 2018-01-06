package demo

import scala.language.higherKinds

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.Graph
import scalax.collection.mutable

import org.scalatest.refspec.RefSpec
import org.scalatest.Matchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** Includes the examples given on [[http://www.scala-graph.org/guides/core-operations.html
  *  Graph Operations]].
  */
@RunWith(classOf[JUnitRunner])
final class EditingTest extends RefSpec with Matchers {

  object `demonstraiting ` {
    def `iterating `: Unit = {
      val g = Graph(2 ~ 3, 3 ~ 1)
      g mkString "-"       // 1-2-3-2~3-3~1
      g.nodes mkString "-" // 1-2-3
      g.edges mkString "-" // 2~3-3~1
    }
    def `looking up`: Unit = {
      val g = mutable.Graph(1~2, 5)
      g find 1                          // Option[g.NodeT] = Some(1)
      g find 3                          // Option[g.NodeT] = None
      g get 1                           // g.NodeT = 1
                                        a [NoSuchElementException] should be thrownBy {
      g get 3                           // NoSuchElementException
                                        }
      g find 1~2                        // Option[g.EdgeT] = Some(1~2)
      g addAndGet 5                     // g.NodeT = 5
      g find (g having (node = _ == 1)) // g.NodeT = 1 TODO
      ()
    }
    def `equality `: Unit = {
      val g = Graph(1 ~ 2)
      (g get 1) == 1 // true
      (g get 1 ~ 2) == 2 ~ 1 // true
      (g get 1 ~ 2) eq 2 ~ 1 // false
      (g get 1 ~ 2) == 2 ~ 2 // false
    }
    def `adding `: Unit = {
      val g = Graph(1, 2 ~ 3) // immutable or mutable
      g + 1 // == g
      g + 0 // Graph(0, 1, 2, 3, 2~3)
//    g + 1.2 // error: overloaded method...
      g + 0 ~ 1 // Graph(0, 1, 2, 3, 0~1, 2~3)
      g ++ List(1 ~ 2, 2 ~ 3) // Graph(1, 2, 3, 1~2, 2~3)
      g ++ List[Param[Int, UnDiEdge]](1 ~ 2, 2 ~ 3, 0) // TODO
      // Graph(0, 1, 2, 3, 1~2, 2~3)
      g - 0 // == g
      g - 1 // Graph(2, 3, 2~3)
      g - 2 // Graph(1, 3)
      g -? 2 // == g
      g - 2 ~ 3 // Graph(1, 2, 3)
      g -! 2 ~ 3 // Graph(1)
      g -- List[Param[Int,UnDiEdge]](2, 3 ~ 3) // Graph(1, 3) // TODO
      val mg = scalax.collection.mutable.Graph(1, 2 ~ 3) // TODO
      // Graph[Int,UnDiEdge]
      // to be assigned before each example
      mg += 0 // Graph(0, 1, 2, 3, 2~3)
//    mg
//    ..= // (mutated graph)
      mg += (3 ~> 1) // Graph(1, 2, 3, 2~3, 3~>1) // TODO
      implicit val factory = scalax.collection.edge.LDiEdge
      mg.addLEdge(3, 4)("red") // true
//    mg // Graph(1, 2, 3, 4, 2~3, 3~>4 'red)
    }
    def `union `: Unit = {
      val g = mutable.Graph(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5) // TODO
      val h = Graph(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)
      g union h // Graph(1~2, 2~3, 2~4, 3~5, 4~5, 3~4, 4~6, 5~6)
      g diff h // Graph(1~2)
      g intersect h // Graph(3~5, 4)
      g &= h // Graph(3~5, 4), same instance
    }
    def `endpoints `: Unit = {
      val uE = 3 ~ 4 // UnDiEdge[Int] = 3~4
      uE._1 * uE._2 // Int = 12
//    uE product // Int = 12 TODO
      uE match {
        case n ~ m => n * m
      } // Int = 12
      val dE = 1 ~> 2 // DiEdge[Int] = 1~>2
      dE.source - dE.target // Int = -1
      uE.arity == dE.arity // Boolean = true
      dE match {
        case s ~> t => s - t
      } // Int = -1
      val hE = 1 ~ 2 ~ 11 ~ 12 // HyperEdge[Int] = 1~2~11~12
      hE._n(hE.arity - 1) // Int = 12
      hE.sum // Int = 26 TODO
    }
    def `edge patterns`: Unit = {
      import scalax.collection.edge.Implicits._
      val g = Graph((1 ~+> 2) ("A"), (1 ~+> 1) ("AB"))

      import scalax.collection.edge.LBase._
      object StringLabel extends LEdgeImplicits[String]
      import StringLabel._

      (0 /: g.edges) ((sum, e) => e.edge match {
        case s :~> t + (l: String) if l contains 'A' =>
          sum + s.outDegree + t.outDegree
      }) // Int = 6
    }
    def `neighbors `: Unit = {
      val g = Graph(0, 1 ~ 3, 3 ~> 2)
      val (n0, n2, n3) = (g get 0, g get 2, g get 3)
      n0.diSuccessors // Set[g.NodeT] = Set() TODO
      n2.diSuccessors.isEmpty // Boolean = true
      n3.diSuccessors // Set[g.NodeT] = Set(1, 2) TODO
      n3.diPredecessors // Set[g.NodeT] = Set(1) TODO
      n2.incoming // Set[g.EdgeT] = Set(3~>2) TODO
      n3 ~>? n2 // Option[g.EdgeT] = Some(3~>2)
    }
    def `querying `: Unit = {
      val g = Graph(2 ~> 3, 3 ~ 1, 5)
      g.nodes filter (_ > 2) // Set[g.NodeT] = Set(5,3)
      g.nodes filter (_.degree > 1) // Set[g.NodeT] = Set(3)
//      g.edges filter (_.diSuccessors.isEmpty) // Set[g.EdgeT] = Set() TODO
      g filter ((i: Int) => i >= 2) // Graph(2,3,5, 2~>3)
      g filter g.having(node = _ >= 2) // Graph(2,3,5, 2~>3)
      g filter g.having(edge = _.directed) // Graph(2,3, 2~>3)
      g count g.having(node = _ >= 3, edge = _.directed) // Int = 3
    }
    def `measuring `: Unit = {
      import scalax.collection.edge.Implicits._ // TODO
      val g = Graph(1~2 % 4, 2~3 % 2, 1~>3 % 5, 1~5  % 3,
        3~5 % 2, 3~4 % 1, 4~>4 % 1, 4~>5 % 0) // TODO
      g.order // Int = 5
      g.graphSize // Int = 8
      g.size // Int = 13
      g.totalDegree // Int = 16
      g.degreeSet // TreeSet(4, 3, 2)
      g.degreeNodeSeq(g.InDegree) // List((4,3), (3,5), (2,1), (2,4), (2,2))
      g.degreeNodesMap // Map(2->Set(2), 3->Set(5,1), 4->Set(3,4))
      g.degreeNodesMap(degreeFilter = _ > 3) // Map(4 -> Set(3,4))
      ()
    }
    def `classifying `: Unit = {
      val g = Graph(1, 2 ~> 3)
      g.isConnected // false
      (g + 2 ~> 1).isConnected // true
      (g get 2).findConnected(_ == 3) // Some(3)
      g.isCyclic // false
      (g + 3 ~> 2).isCyclic // true
      g.isComplete // false
      (g ++ List(1 ~> 2, 1 ~> 3, 2 ~> 1, 3 ~> 1, 3 ~> 2)).isComplete // true
      g.isDirected // true
      g.isHyper // false
      g.isMulti // false
    }
  }
}
