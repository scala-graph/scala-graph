package scalax.collection

import scala.language.postfixOps
import scala.collection.mutable.{ListBuffer, Map => MutableMap}
import scala.concurrent.{future, Future, Await}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.util.Random

import GraphPredef._, GraphEdge._
import generator.{RandomGraph, NodeDegreeRange}

import org.scalatest.Spec
import org.scalatest.Informer
import org.scalatest.Matchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** Ensure that stateful data handling used for traversals is thread-safe. 
 */
@RunWith(classOf[JUnitRunner])
class TStateTest extends Spec with Matchers {
  val g = Graph(Data.elementsOfUnDi_2: _*)

  // a sample traversal with recursive calls in its node visitor
  def countNodes(recursion: Int = 0): Int = {
    assert(recursion >= 0)
    var nrNodes = 0
    g.nodes.head.innerNodeTraverser foreach { _ =>
      nrNodes += (
        if (recursion == 0) 1
        else countNodes(recursion - 1)
      )
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
    g.nodes.head.innerNodeTraverser foreach ( _ =>
      test_Loop
    )
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
      g.nodes.head.innerNodeTraverser foreach ( n =>
        nrNodes += (
          // if (n eq recurseAt) println(State.dump(recurseAt).summary)
          if (recursion == 0) 0
          else if (n eq recurseAt) countNodesDeep(recursion - 1)
          else 1
        )
      )
      nrNodes
    }
    for(i <- 1 to 2) countNodesDeep(aLotOfTimes)
  }
  def test_Futures {
    val traversals = Future.sequence(
        for (i <- 1 to aLotOfTimes)
        yield future { countNodes() }
    )
    // statistics map with key = nrOfNodesCounted, value = frequency
    val stat = MutableMap.empty[Int,Int]
    val a = Await.result(traversals, 1 seconds) foreach { cnt =>
      stat += cnt -> (stat.getOrElse(cnt, 0) + 1)
    }
    // each traversal must yield the same result
    stat should be (Map(nrNodesExpected -> aLotOfTimes))
  }
  def test_clear {
    /* State.clearNodeStates could cause NPE after lots of unconnected traversals.
     */
    val order = 5000
    val r = new Random(10 * order)
    def intNodeFactory = r.nextInt
    val g = new RandomGraph[Int,DiEdge,Graph](
        Graph,
        order, 
        intNodeFactory, 
        NodeDegreeRange(0,2), 
        Set(DiEdge),
        false) {
      val graphConfig = graphCompanion.defaultConfig
    }.draw
    val rootNodes = List.fill(50)(g.nodes.draw(r))
    for {node <- g.nodes
         root <- rootNodes}
      node.hasSuccessor(root)
  }
}