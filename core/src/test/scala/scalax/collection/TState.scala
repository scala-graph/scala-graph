package scalax.collection

import scala.language.postfixOps
import scala.collection.mutable.{Map => MutableMap}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.util.Random

import GraphPredef._, GraphEdge._
import generator.{NodeDegreeRange, RandomGraph}

import org.scalatest.refspec.RefSpec
import org.scalatest.Matchers

/** Ensure that stateful data handling used for traversals is thread-safe.
  */
class TStateTest extends RefSpec with Matchers {
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
  val aLotOfTimes     = 162 // at least 2 times State.nrOfFlagWordBits

  def dump {
    println(State.dump(g))
  }

  object `Single-threaded shared state proves robust` {
    def `when looping` {
      for (i <- 1 to aLotOfTimes)
        countNodes() should be(nrNodesExpected)
    }
    def `when looping at visited nodes` {
      g.nodes.head.innerNodeTraverser foreach (_ => `when looping`)
    }
    def `when called recursively` {
      val depth = 5
      countNodes(depth) should be(math.pow(3, depth) * nrNodesExpected)
    }
    def `when called deep-recursively` {
      val recurseAt = g.nodes.head
      def countNodesDeep(recursion: Int): Int = {
        assert(recursion >= 0)
        var nrNodes = 0
        g.nodes.head.innerNodeTraverser foreach (n =>
          nrNodes += (
            // if (n eq recurseAt) println(State.dump(recurseAt).summary)
            if (recursion == 0) 0
            else if (n eq recurseAt) countNodesDeep(recursion - 1)
            else 1
          ))
        nrNodes
      }
      for (i <- 1 to 2) countNodesDeep(aLotOfTimes)
    }
    def `when cleared up after lots of unconnected traversals` {
      val order          = 5000
      val r              = new Random(10 * order)
      def intNodeFactory = r.nextInt
      val g =
        new RandomGraph[Int, DiEdge, Graph](Graph, order, intNodeFactory, NodeDegreeRange(0, 2), Set(DiEdge), false) {
          val graphConfig = graphCompanion.defaultConfig
        }.draw
      val rootNodes = List.fill(50)(g.nodes.draw(r))
      for {
        node <- g.nodes
        root <- rootNodes
      } node.hasSuccessor(root)
    }
  }
  object `Multi-threaded shared state proves robust` {
    def `when traversing by futures` {
      val traversals = Future.sequence(
        for (i <- 1 to aLotOfTimes)
          yield Future { countNodes() }
      )
      // statistics map with key = nrOfNodesCounted, value = frequency
      val stat = MutableMap.empty[Int, Int]
      Await.result(traversals, 1 seconds) foreach { cnt =>
        stat += cnt -> (stat.getOrElse(cnt, 0) + 1)
      }
      // each traversal must yield the same result
      stat should be(Map(nrNodesExpected -> aLotOfTimes))
    }
    /* TODO value par is not a member of ...
    def `when tested under stress fixing #34` {
      import Data._
      object g extends TGraph[Int, DiEdge, Graph](Graph(elementsOfDi_1: _*))
      def n(outer: Int) = g.node(outer)
      val (n1, n2)      = (n(2), n(5))

      val times = 200000
      def run: Boolean =
        (1 to times).par forall { i =>
          (n1 pathTo n2).nonEmpty
        }
      for (i <- 1 to 3)
        run should be(true)
    }
    */
  }
}
