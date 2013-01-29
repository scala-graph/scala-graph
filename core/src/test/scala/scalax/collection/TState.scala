package scalax.collection

import org.scalatest.Suite
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import collection.mutable.{ListBuffer, Map => MutableMap}
import actors.{Future, Futures}, Futures._

import GraphPredef._, GraphEdge._
import GraphTraversal.VisitorReturn._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** Ensure that stateful data handling used for traversals is thread-safe. 
 */
@RunWith(classOf[JUnitRunner])
class TStateTest extends Suite with ShouldMatchers {
  def test_Futures {
    val g = Graph(Data.elementsOfUnDi_2: _*)
    val n1 = g get 1
  
    // a sample traversal
    def countNodes: Int = {
      var nrNodes = 0
      n1.traverseNodes() { _ =>
        nrNodes += 1 
        GraphTraversal.VisitorReturn.Continue
      }
      nrNodes
    }
    val nrNodesExpected = countNodes
    val aLotOfTimes = 150 // at least 2 times GraphTraversalImpl.State.untilBit
    val traversals = for (i <- 1 to aLotOfTimes)
                       yield future { countNodes }
    // statistics map with key = nrOfNodesCounted, value = frequency
    val stat = MutableMap.empty[Int,Int] 
    awaitAll(1000, traversals: _*) foreach { opt =>
      opt.getOrElse(-1) match {
        case cnt: Int => stat += cnt -> (stat.getOrElse(cnt, 0) + 1)
      }
    }
    // each traversal must yield the same result
    stat should be (Map(nrNodesExpected -> aLotOfTimes))
  }
}