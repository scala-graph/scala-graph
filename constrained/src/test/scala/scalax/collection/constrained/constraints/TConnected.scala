package scalax.collection.constrained
package constraints

import scala.language.higherKinds

import org.scalatest._
import org.scalatest.refspec.RefSpec
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import generic.GraphConstrainedCompanion
import scalax.collection.mutable.{Graph => SimpleGraph}

class TConnectedRootTest
    extends Suites(
      new TConnected[immutable.Graph](immutable.Graph),
      new TConnected[mutable.Graph](mutable.Graph),
      new TConnectedMutable)

class TConnectedMutable extends RefSpec with Matchers with Testing[mutable.Graph] {
  val factory = mutable.Graph

  implicit val config: Config = Connected

  object `The 'Connected' constraint works fine with mutable graphs on` {
    def `adding nodes or edges` {
      val init    = Seq(1 ~> 2, 2 ~> 3)
      val simpleG = SimpleGraph(init: _*)
      val g       = factory(init: _*)

      given(g, 4) both (_ += _, _ +=? _) should equal(simpleG)
      given(g, 4 ~> 5) both (_ += _, _ +=? _) should equal(simpleG)
      given(g, Seq(4 ~> 5, 5 ~> 6)) both (_ ++= _, _ ++=? _) should equal(simpleG)

      val newEdge = 4 ~> 3
      given(g, newEdge) both (_ += _, _ +=? _) should equal(simpleG.clone += newEdge)

      val newElems = Seq(4 ~> 5, 5 ~> 6, 6 ~> 1)
      given(g, newElems) both (_ ++= _, _ ++=? _) should equal(simpleG.clone ++= newElems)
    }

    def `substracting nodes or edges` {
      val (e1, e2, e3) = (1 ~ 2, 2 ~ 3, 3 ~ 4)
      val init         = Seq(e1, e2, e3)
      val simpleG      = SimpleGraph(init: _*)
      val g            = factory(init: _*)

      given(g, e2) both (_ -= _, _ -=? _) should equal(simpleG)
      given(g, List(e2, e3)) both (_ --= _, _ --=? _) should equal(simpleG)

      /* It is planned to drop support for -=!
      (g -!= e1) should be(simpleG -!= e1)
       */

      val minusNodes = List(1, 2).toOuterNodes[UnDiEdge]
      given(g, minusNodes) both (_ --= _, _ --=? _) should equal(simpleG.clone --= minusNodes)

      /* It is planned to drop support for -=!
      val minusEdges = List(e2, e3)
      (g --!= minusEdges) should be(simpleG --!= minusEdges)
       */

      val minusElems = List[InParam[Int, UnDiEdge]](1 ~ 2, 1)
      given(g, minusElems) both (_ --= _, _ --=? _) should equal(simpleG.clone --= minusElems)
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
      val g1 = factory[Int, DiEdge](1 ~> 2, 2 ~> 3)
      g1 should have size (5)
      val g2 = factory[Int, DiEdge](1 ~> 2, 3 ~> 4)
      g2 should have size (0)
    }
    def `adding nodes or edges` {
      type Di = Graph[Int, DiEdge]

      val init    = Seq(1 ~> 2, 2 ~> 3)
      val simpleG = factory[Int, DiEdge](init: _*)
      val g       = simpleG

      given(g, 4) both (_ + _, _ +? _) should meet((_: Di) === g)

      given(g, 4 ~> 5) both (_ + _, _ +? _) should meet((_: Di) === g)
      val newEdge = 4 ~> 3
      given(g, newEdge) both (_ + _, _ +? _) should meet((_: Di) === simpleG + newEdge)

      given(g, Seq(4 ~> 5, 5 ~> 6)) both (_ ++ _, _ ++? _) should meet((_: Di) === g)
      val newElems = Seq(4 ~> 5, 5 ~> 6, 6 ~> 1)
      given(g, newElems) both (_ ++ _, _ ++? _) should meet((_: Di) === simpleG ++ newElems)
    }
    def `substracting nodes or edges` {
      type UnDi = Graph[Int, UnDiEdge]

      val (e1, e2, e3) = (1 ~ 2, 2 ~ 3, 3 ~ 4)
      val init         = Seq(e1, e2, e3)
      val simpleG      = factory[Int, UnDiEdge](init: _*)

      val g = simpleG
      g should have size (7)

      given(g, e2) both (_ - _, _ -? _) should equal(g)
      given(g, e1) both (_ - _, _ -? _) should equal(g)
      g -! e1 should be(simpleG -! e1)

      given(g, List(e2, e3)) both (_ -- _, _ --? _) should equal(g)
      val minusNodes = List(1, 2).toOuterNodes[UnDiEdge]
      given(g, minusNodes) both (_ -- _, _ --? _) should equal(g -- minusNodes)
      val minusEdges = List(e2, e3)
      g --! minusEdges should be(simpleG --! minusEdges)
    }
  }
}
