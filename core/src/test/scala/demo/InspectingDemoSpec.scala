package demo

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.mutable.Graph

class InspectingDemoSpec extends RefSpec with Matchers {

  object `demonstrating ` {

    def `Iterating`(): Unit = {
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

    def `Looking up Nodes and Edges`(): Unit = {
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
  }
}
