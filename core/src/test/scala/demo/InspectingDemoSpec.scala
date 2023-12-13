package demo

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OuterImplicits._
import scalax.collection.{OneOrMore, Several}
import scalax.collection.edges._
import scalax.collection.generic.AnyEdge
import scalax.collection.hyperedges._
import scalax.collection.mutable.Graph

class InspectingDemoSpec extends RefSpec with Matchers {

  object `demonstrating ` {

    def `Iterate over Elements`(): Unit = {
      val g = Graph(2 ~ 3, 3 ~ 1)
      g.toString            // "Graph(NodeSet(1, 2, 3), EdgeSet(2 ~ 3, 3 ~ 1))"
      g.nodes mkString ", " // "1, 2, 3"
      g.edges mkString ", " // "2 ~ 3, 3 ~ 1"
      g.outerIterator
        .map {
          case g.OuterNode(n) => n.toString
          case g.OuterEdge(e) => e.toString
        }
        .mkString(", ") // 1, 2, 3, 2 ~ 3, 3 ~ 1
    }

    def `Look up Nodes and Edges`(): Unit = {
      val g = Graph(1 ~ 2)
      g find 1 // Option[g.NodeT] = Some(1)
      g find 3 // Option[g.NodeT] = None
      g get 1  // g.NodeT = 1
      a[NoSuchElementException] should be thrownBy {
        g get 3
      }                     // NoSuchElementException
      g find 1 ~ 2          // Option[g.EdgeT] = Some(1 ~ 2)
      g.nodes find (_ == 1) // Option[g.NodeT] = 1
      g addAndGet 3         // g.NodeT = 3
    }

    def `Inspect Edge Ends`(): Unit = {
      val unDiEdge = 3 ~ 4 // UnDiEdge[Int]
      unDiEdge.node1 * unDiEdge.node2 // Int = 12
      unDiEdge.ends.iterator.product  // Int = 12
      unDiEdge match {
        case n ~ m => n * m // Int = 12
      }

      val diEdge = 1 ~> 2 // DiEdge[Int]
      unDiEdge.arity == diEdge.arity // true
      diEdge.source - diEdge.target  // Int = -1
      diEdge match {
        case s ~> t => s - t // Int = -1
      }

      val hyperEdge = 1 ~~ 2 ~~ 11 ~~ 12 // HyperEdge[Int]
      hyperEdge.ends.get(hyperEdge.arity - 1) // Int = 12
      hyperEdge.ends.iterator.sum             // Int = 26
      hyperEdge match {
        case ~~(Several.Seq(n1, n2, _*)) => n1 - n2 // Int = -1
      }

      val diHyperEdge = OneOrMore(1, 2) ~~> OneOrMore(5, 6)
      diHyperEdge.sources.iterator.sum // Int = 3
      diHyperEdge.targets.iterator.sum // Int = 11
      diHyperEdge match {
        case OneOrMore.Seq(_, s2, _*) ~~> OneOrMore(t1, _) => s2 * t1 // Int = 10
      }

      /* TODO above yields warning "match may not be exhaustive. It would fail on the following input: HyperEdge(_)"
         below does not:
      val it = Iterable(1, 2)
      val x = it match {
        case Seq(i1, _*) => i1
      } */
    }

    def `Inspect Neighbors and Incident Edges`(): Unit = {
      val g = Graph[Int, AnyEdge](0, 1 ~ 3, 3 ~> 2)

      def n(outer: Int): g.NodeT = g get outer

      n(0).diSuccessors   // Set[g.NodeT] = Set()
      n(2).diSuccessors   // Set[g.NodeT] = Set()
      n(3).diSuccessors   // Set[g.NodeT] = Set(1, 2)
      n(3).diPredecessors // Set[g.NodeT] = Set(1)
      n(2).incoming       // Set[g.EdgeT] = Set(3 ~> 2)
    }
  }

  def `Query the Node or Edge Set`(): Unit = {
    val g = Graph[Int, AnyEdge](2 ~> 3, 3 ~ 1, 5)

    g.nodes.filter(_.outer > 2)         // Set[g.NodeT] = Set(3, 5)
    g.nodes.filter(_.degree > 1)        // Set[g.NodeT] = Set(3)
    g.edges.filter(_.adjacents.isEmpty) // Set[g.EdgeT] = Set()

    g.outerIterator
      .filter {
        case g.OuterNode(n)               => n > 1
        case g.OuterEdge(AnyEdge(n1, n2)) => n1 > 1 && n2 > 1
      }
      .map {
        case g.OuterNode(n) => n
        case g.OuterEdge(e) => e
      }
      .toList // List[Any] = List(2, 3, 5, 2 ~> 3)

    g.iterator.filter {
      case g.InnerNode(innerN, _) => innerN.degree > 1
      case g.InnerEdge(_, outerE) => outerE.isDirected
    }.mkString // String = 3, 2 ~> 3
  }

  def `Compute Union, Difference and Intersection`(): Unit = {
    val g = Graph(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
    val h = Graph(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)
    g union h     // Graph(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5, 3 ~ 4, 4 ~ 6, 5 ~ 6)
    g diff h      // Graph(1 ~ 2)
    g intersect h // Graph(4, 3 ~ 5)
    g &= h        // Graph(4, 3 ~ 5), mutated instance
  }

  def `Measure your Graph`(): Unit = {
    val g = Graph[Int, AnyEdge](1 ~ 2, 2 ~ 3, 1 ~> 3, 1 ~ 5, 3 ~ 5, 3 ~ 4, 4 ~> 4, 4 ~> 5)

    g.order                                // Int = 5
    g.size                                 // Int = 8
    g.elementCount                         // Int = 13
    g.totalDegree                          // Int = 16
    g.degreeSet                            // TreeSet(4, 3, 2)
    g.degreeNodeSeq(g.InDegree)            // List((4, 3), (3, 5), (2, 1), (2, 2), (2, 4))
    g.degreeNodesMap                       // Map(2->Set(2), 3->Set(5, 1), 4->Set(3, 4))
    g.degreeNodesMap(degreeFilter = _ > 3) // Map(4 -> Set(3, 4))
  }

  def `Classify your Graph`(): Unit = {
    val g = Graph(1, 2 ~> 3)

    g.isConnected                                                  // false
    (g addOne 2 ~> 1).isConnected                                  // true
    (g get 2).findConnected(_.outer == 3)                          // Some(3)
    g.isCyclic                                                     // false
    (g addOne 3 ~> 2).isCyclic                                     // true
    g.isComplete                                                   // false
    (g ++ List(1 ~> 2, 1 ~> 3, 2 ~> 1, 3 ~> 1, 3 ~> 2)).isComplete // true
    g.isDirected                                                   // true
    g.isHyper                                                      // false
    g.isMulti                                                      // false
  }
}
