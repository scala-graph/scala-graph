package scalax.collection

import language.higherKinds

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.visualization.Visualizer

@RunWith(classOf[JUnitRunner])
class TOpRootTest
    extends Suites(new TOp[immutable.Graph](immutable.Graph), new TOp[mutable.Graph](mutable.Graph), new TMutableOp)

class TOp[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers
    with Visualizer[CC] {

  val g = factory(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  val h = factory(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

  def `union ` {
    val expected = factory(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5, 3 ~ 4, 4 ~ 6, 5 ~ 6)
    given(g union h) { _ should be(expected) }
    given(g ++ h) { _ should be(expected) }
  }
  def `difference ` {
    val expected = factory(1 ~ 2)
    given(g diff h) { _ should be(expected) }
    given(g -- h) { _ should be(expected) }
  }
  def `intersection ` {
    val expected = factory(3 ~ 5, 4)
    given(g intersect h) { _ should be(expected) }
    given(g & h) { _ should be(expected) }
  }
}

class TMutableOp extends RefSpec with Matchers {

  val oEdgesG              = List[UnDiEdge[Int]](1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  val oEdgesH              = List[UnDiEdge[Int]](3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)
  val (iFactory, mFactory) = (immutable.Graph, mutable.Graph)
  val none                 = Set.empty
  def initG                = (iFactory.from(none, oEdgesG), mFactory.from(none, oEdgesG))
  def initH                = (iFactory.from(none, oEdgesH), mFactory.from(none, oEdgesH))
  def ` union` {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5, 3 ~ 4, 4 ~ 6, 5 ~ 6)
    (mG ++= mH) should be(expected)
    (mG ++= iH) should be(expected)
  }
  def `difference ` {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(1 ~ 2)
    (mG --= mH) should be(expected)
    (mG --= iH) should be(expected)
  }
  def `intersection ` {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(3 ~ 5, 4)
    (mG &= mH) should be(expected)
    (mG &= iH) should be(expected)
  }
}
