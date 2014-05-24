package scalax.collection.constrained
package constraints

import scala.language.{higherKinds, postfixOps}

import org.scalatest.{Spec, Suites}
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.{Graph => SimpleGraph}
import PreCheckFollowUp._
import generic.GraphConstrainedCompanion

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TConnectedRootTest
  extends Suites(
    new TConnected[immutable.Graph](immutable.Graph),
    new TConnected[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  import mutable.Graph
  import scalax.collection.mutable.{Graph => SimpleGraph}

  implicit val config: Config = Connected 
  def test_mutableAdd {
    val init = Seq(1~>2, 2~>3)
    val simpleG = SimpleGraph(init: _*)
    val g = Graph(init: _*)

    (g += 4)    should be (simpleG)
    (g += 4~>5) should be (simpleG)
    (g ++= Seq(4~>5, 5~>6)) should be (simpleG)

    val newEdge = 4~>3
    (g += newEdge) should be (simpleG += newEdge)

    val newElems = Seq(4~>5, 5~>6, 6~>1)
    (g ++= newElems) should be (simpleG ++= newElems)
  }
  def test_mutableSubtract {
    val (e1, e2, e3) = (1~2, 2~3, 3~4)
    val init = Seq(e1, e2, e3)
    val simpleG = SimpleGraph(init: _*)
    val g = Graph(init: _*)

    (g -=  e2) should be (simpleG)
    (g -=  e1) should be (simpleG)
    (g --=  List(e2, e3)) should be (simpleG)

    (g -!= e1) should be (simpleG -!= e1)
    val minusNodes = List(1, 2)
    (g --=  minusNodes)   should be (simpleG --= minusNodes)
    val minusEdges = List(e2, e3)
    (g --!= minusEdges) should be (simpleG --!= minusEdges)

    (g ++= init) should be (simpleG ++= init)
    val minus = List[InParam[Int,UnDiEdge]](1~2, 1)
    (g --=  minus)   should be (simpleG --= minus)
  }
}

class TConnected [CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphConstrainedCompanion[CC])
  extends Spec
  with    ShouldMatchers
{
  implicit val config: Config = Connected 
  def test_0(info : Informer) {
    info("factory = " + factory.getClass)
  }
  def test_create {
    val g1 = Graph(1~>2, 2~>3)
    g1 should have size (5)
    val g2 = Graph(1~>2, 3~>4)
    g2 should have size (0)
  }
  def test_add {
    val init = Seq(1~>2, 2~>3)
    val simpleG = SimpleGraph(init: _*)
    val g = Graph(init: _*)

    g + 4 should be (g)

    g + 4~>5    should be (g)
    val newEdge = 4~>3
    g + newEdge should be (simpleG + newEdge)

    g ++ Seq(4~>5, 5~>6) should be (g)
    val newElems = Seq(4~>5, 5~>6, 6~>1)
    g ++ newElems should be (simpleG ++ newElems)
  }
  def test_subtract {
    val (e1, e2, e3) = (1~2, 2~3, 3~4)
    val init = Seq(e1, e2, e3)
    val simpleG = SimpleGraph(init: _*)

    val g = Graph(init: _*)
    g should have size (7)

    g -  e2 should be (g)
    g -  e1 should be (g)
    g -! e1 should be (simpleG -! e1)

    g --  List(e2, e3) should be (g)
    val minusNodes = List(1, 2)
    g --  minusNodes   should be (simpleG -- minusNodes)
    val minusEdges = List(e2, e3)
    g --! minusEdges should be (simpleG --! minusEdges)
  }
}