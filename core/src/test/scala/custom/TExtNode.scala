package custom

import language.postfixOps

import scalax.collection.GraphPredef._
import immutable.{MyExtGraph => Graph}
import mutable.{MyExtGraph => MutableGraph}

import org.scalatest.refspec.RefSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TExtNodeTest extends RefSpec with Matchers {

  object `inner node enriched by 'helloSuccessors' works for` {

    def `immutable graphs`: Unit = {
      val g = Graph(1 ~ 2, 1 ~ 3)
      (g get 1 helloSuccessors) should be("Hello 2,3!")
    }

    def `mutable graphs`: Unit = {
      val g  = MutableGraph(1 ~ 2, 1 ~ 3)
      val n1 = g get 1
      (n1 helloSuccessors) should be("Hello 2,3!")
      g += 1 ~ 4
      (n1 helloSuccessors) should be("Hello 2,3,4!")
    }
  }
}
