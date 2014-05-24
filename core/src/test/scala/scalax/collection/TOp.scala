package scalax.collection

import language.{higherKinds, postfixOps}

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest.Spec
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TOpRootTest
  extends Suites(
      new TOp[immutable.Graph](immutable.Graph),
      new TOp[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers {
  // ---------------------------------------- mutable tests
  val oEdgesG = List[UnDiEdge[Int]](1~2, 2~3, 2~4, 3~5, 4~5)
  val oEdgesH = List[UnDiEdge[Int]](3~4, 3~5, 4~6, 5~6)
  val (iFactory, mFactory) = (immutable.Graph,
                                mutable.Graph)
  val none = Set.empty
  def initG = (iFactory.from(none, oEdgesG), mFactory.from(none, oEdgesG))
  def initH = (iFactory.from(none, oEdgesH), mFactory.from(none, oEdgesH))
  def test_union {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(1~2, 2~3, 2~4, 3~5, 4~5, 3~4, 4~6, 5~6)
    (mG ++= mH) should be (expected)
    (mG ++= iH) should be (expected)
  }
  def test_difference {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(1~2)
    (mG --= mH) should be (expected)
    (mG --= iH) should be (expected)
  }
  def test_intersection {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(3~5, 4)
    (mG &= mH) should be (expected)
    (mG &= iH) should be (expected)
  }
}
/**	This class contains tests for graph operations.
 */
class TOp[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphCoreCompanion[CC])
	extends	Spec
	with	ShouldMatchers
{
  val g = factory(1~2, 2~3, 2~4, 3~5, 4~5)
  val h = factory(3~4, 3~5, 4~6, 5~6)

  def test_union {
    val expected = factory(1~2, 2~3, 2~4, 3~5, 4~5, 3~4, 4~6, 5~6)
    g union h should be (expected)
    g ++ h    should be (expected)
  }
  def test_difference {
    val expected = factory(1~2)
    g diff h should be (expected)
    g -- h   should be (expected)
  }
  def test_intersection {
    val expected = factory(3~5, 4)
    g intersect h should be (expected)
    g & h         should be (expected)
  }
}