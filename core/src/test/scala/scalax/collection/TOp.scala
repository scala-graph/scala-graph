package scalax.collection

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest._
import org.scalatest.matchers.should
import org.scalatest.refspec.RefSpec

import scalax.collection.visualization.Visualizer

class TOpRootTest
    extends Suites(new TOp[immutable.Graph](immutable.Graph), new TOp[mutable.Graph](mutable.Graph), new TMutableOp)

class TOp[CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with should.Matchers
    with Visualizer[CC] {

  val g: CC[Int, UnDiEdge] = factory(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  val h: CC[Int, UnDiEdge] = factory(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

  def `union ` : Unit = {
    val expected = factory(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5, 3 ~ 4, 4 ~ 6, 5 ~ 6)
    given(g union h)(_ should be(expected))
    given(g ++ h)(_ should be(expected))
  }

  def `difference ` : Unit = {
    val expected = factory(1 ~ 2)
    given(g diff h)(_ should be(expected))
    given(g -- h)(_ should be(expected))
  }

  def `intersection ` : Unit = {
    val expected = factory(3 ~ 5, 4)
    given(g intersect h)(_ should be(expected))
    given(g & h)(_ should be(expected))
  }
}

class TMutableOp extends RefSpec with should.Matchers {

  val oEdgesG: List[UnDiEdge[Int]] = List[UnDiEdge[Int]](1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  val oEdgesH: List[UnDiEdge[Int]] = List[UnDiEdge[Int]](3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)
  val (iFactory, mFactory)         = (immutable.Graph, mutable.Graph)
  val none                         = Set.empty

  def initG: (immutable.Graph[Int, UnDiEdge], mutable.Graph[Int, UnDiEdge]) =
    (iFactory.from(none, oEdgesG), mFactory.from(none, oEdgesG))

  def initH: (immutable.Graph[Int, UnDiEdge], mutable.Graph[Int, UnDiEdge]) =
    (iFactory.from(none, oEdgesH), mFactory.from(none, oEdgesH))

  def ` union`: Unit = {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5, 3 ~ 4, 4 ~ 6, 5 ~ 6)
    (mG ++= mH) should be(expected)
    (mG ++= iH) should be(expected)
  }

  def `difference ` : Unit = {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(1 ~ 2)
    (mG --= mH) should be(expected)
    (mG --= iH) should be(expected)
  }

  def `intersection ` : Unit = {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(3 ~ 5, 4)
    (mG &= mH) should be(expected)
    (mG &= iH) should be(expected)
  }
}
