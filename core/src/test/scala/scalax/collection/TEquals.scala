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
class TEqualsRootTest
  extends Suites(
      new TEquals[immutable.Graph](immutable.Graph),
      new TEquals[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  // ---------------------------------------- mutable tests
  val oEdgesG = List[UnDiEdge[Int]](1~2, 2~3, 2~4, 3~5, 4~5)
  val oEdgesH = List[UnDiEdge[Int]](3~4, 3~5, 4~6, 5~6)
  val (iFactory, mFactory) = (immutable.Graph,
                                mutable.Graph)
  val none = Set.empty
  def initG = (iFactory.from(none, oEdgesG), mFactory.from(none, oEdgesG))
  def initH = (iFactory.from(none, oEdgesH), mFactory.from(none, oEdgesH))

  def test_equals {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val ok = iG == mG
    ok should be (true)
  }
}
/**	This class contains tests for graph operations.
 */
class TEquals[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphCoreCompanion[CC])
	extends	Spec
	with	ShouldMatchers
{
  val g = factory(1~2, 2~3, 2~4, 3~5, 4~5)
  val h = factory(3~4, 3~5, 4~6, 5~6)
}