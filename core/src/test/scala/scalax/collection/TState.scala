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
  val g = Graph(Data.elementsOfUnDi_2: _*)

  // a sample traversal with recursive calls in its node visitor
  def countNodes(recursion: Int = 0): Int = {
    assert(recursion >= 0)
    var nrNodes = 0
    g.nodes.head.traverseNodes() { _ =>
      nrNodes += {
        if (recursion == 0) 1
        else countNodes(recursion - 1)
      }
      Continue
    }
    nrNodes
  }
  val nrNodesExpected = g.order
  val aLotOfTimes = 162 // at least 2 times State.nrOfFlagWordBits

  def dump {
    println(State.dump(g))
  }
  def test_Loop {
    for (i <- 1 to aLotOfTimes)
      countNodes() should be (nrNodesExpected)
  }
  def test_InnerLoop {
    g.nodes.head.traverseNodes() { _ =>
      test_Loop
      Continue
    }
  }
  def test_Recursion {
    val depth = 5
    countNodes(depth) should be (math.pow(3, depth) * nrNodesExpected)
  }
  def test_DeepRecursion {
    val recurseAt = g.nodes.head 
    def countNodesDeep(recursion: Int): Int = {
      assert(recursion >= 0)
      var nrNodes = 0
      g.nodes.head.traverseNodes() { n =>
        nrNodes += {
          // if (n eq recurseAt) println(State.dump(recurseAt).summary)
          if (recursion == 0) 0
          else if (n eq recurseAt) countNodesDeep(recursion - 1)
          else 1
        }
        Continue
      }
      nrNodes
    }
    for(i <- 1 to 2) countNodesDeep(aLotOfTimes)
  }
  def test_Futures {
    val traversals = for (i <- 1 to aLotOfTimes)
                       yield future { countNodes() }
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