package scalax.collection.constrained
package constraints

import scala.language.{higherKinds, postfixOps}

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.{Graph => SimpleGraph}
import PreCheckFollowUp._
import generic.GraphConstrainedCompanion

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TConnectedRootTest
    extends Suites(
      new TConnected[immutable.Graph](immutable.Graph),
      new TConnected[mutable.Graph](mutable.Graph),
      new TConnectedMutable)

class TConnectedMutable extends RefSpec with Matchers with Testing[mutable.Graph] {
  val factory = mutable.Graph

  import mutable.Graph
  import scalax.collection.mutable.{Graph => SimpleGraph}

  implicit val config: Config = Connected

  object `The 'Connected' constraint works fine with mutable graphs on` {
    def `adding nodes or edges` {
      val init    = Seq(1 ~> 2, 2 ~> 3)
      val simpleG = SimpleGraph(init: _*)
      val g       = Graph(init: _*)

      given(g, 4) both (_ += _, _ +=? _) should meet((_: Graph[Int, DiEdge]) === simpleG)
      given(g, 4 ~> 5) both (_ += _, _ +=? _) should meet((_: Graph[Int, DiEdge]) === simpleG)
      given(g, Seq(4 ~> 5, 5 ~> 6)) both (_ ++= _, _ ++=? _) should meet((_: Graph[Int, DiEdge]) === simpleG)

      val newEdge = 4 ~> 3
      given(g, newEdge) both (_ += _, _ +=? _) should meet((_: Graph[Int, DiEdge]) === (simpleG += newEdge))

      val newElems = Seq(4 ~> 5, 5 ~> 6, 6 ~> 1)
      given(g, newElems) both (_ ++= _, _ ++=? _) should meet((_: Graph[Int, DiEdge]) === (simpleG ++= newElems))
    }
    def `substracting nodes or edges` {
      val (e1, e2, e3) = (1 ~ 2, 2 ~ 3, 3 ~ 4)
      val init         = Seq(e1, e2, e3)
      val simpleG      = SimpleGraph(init: _*)
      val g            = Graph(init: _*)

      given(g, e2) both (_ -= _, _ -=? _) should meet((_: Graph[Int, UnDiEdge]) === simpleG)
      given(g, e1) both (_ -= _, _ -=? _) should meet((_: Graph[Int, UnDiEdge]) === simpleG)
      given(g, List(e2, e3)) both (_ --= _, _ --=? _) should meet((_: Graph[Int, UnDiEdge]) === simpleG)

      (g -!= e1) should be(simpleG -!= e1)
      val minusNodes = List(1, 2).toOuterNodes[UnDiEdge]
      given(g, minusNodes) both (_ --= _, _ --=? _) should meet((_: Graph[Int, UnDiEdge]) === (simpleG --= minusNodes))
      val minusEdges = List(e2, e3)
      (g --!= minusEdges) should be(simpleG --!= minusEdges)

      given(g, init) both (_ ++= _, _ ++=? _) should meet((_: Graph[Int, UnDiEdge]) === (simpleG ++= init))
      val minus = List[InParam[Int, UnDiEdge]](1 ~ 2, 1)
      given(g, minus) both (_ --= _, _ --=? _) should meet((_: Graph[Int, UnDiEdge]) === (simpleG --= minus))
    }
  }
}

class TConnected[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphConstrainedCompanion[CC])
    extends RefSpec
    with Matchers
    with Testing[CC] {

  implicit val config: Config = Connected

  info("factory = " + factory.getClass)

  object `The 'Connected' constraint works fine with graphs on` {
    def `creation ` {
      val g1 = Graph(1 ~> 2, 2 ~> 3)
      g1 should have size (5)
      val g2 = Graph(1 ~> 2, 3 ~> 4)
      g2 should have size (0)
    }
    def `adding nodes or edges` {
      val init    = Seq(1 ~> 2, 2 ~> 3)
      val simpleG = SimpleGraph(init: _*)
      val g       = Graph(init: _*)

      g + 4 should be(g)

      g + 4 ~> 5 should be(g)
      val newEdge = 4 ~> 3
      g + newEdge should be(simpleG + newEdge)

      g ++ Seq(4 ~> 5, 5 ~> 6) should be(g)
      val newElems = Seq(4 ~> 5, 5 ~> 6, 6 ~> 1)
      g ++ newElems should be(simpleG ++ newElems)
    }
    def `substracting nodes or edges` {
      val (e1, e2, e3) = (1 ~ 2, 2 ~ 3, 3 ~ 4)
      val init         = Seq(e1, e2, e3)
      val simpleG      = SimpleGraph(init: _*)

      val g = Graph(init: _*)
      g should have size (7)

      g - e2 should be(g)
      g - e1 should be(g)
      g -! e1 should be(simpleG -! e1)

      g -- List(e2, e3) should be(g)
      val minusNodes = List(1, 2).toOuterNodes[UnDiEdge]
      g -- minusNodes should be(simpleG -- minusNodes)
      val minusEdges = List(e2, e3)
      g --! minusEdges should be(simpleG --! minusEdges)
    }
  }
}
