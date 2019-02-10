package scalax.collection

import language.{higherKinds, postfixOps}

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.visualization.Visualizer

@RunWith(classOf[JUnitRunner])
class TOpRootTest
    extends Suites(
      new TOp[immutable.Graph](immutable.Graph),
      new TOp[mutable.Graph](mutable.Graph),
      new TImmutableOp,
      new TMutableOp
    )

protected trait Examples[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]] {

  protected def factory: GraphCoreCompanion[CC]

  protected lazy val g = factory(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  protected lazy val h = factory(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

  protected object Expected {
    val g_union_h = factory(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5, 3 ~ 4, 4 ~ 6, 5 ~ 6)
    val g_diff_h  = factory(1 ~ 2)
  }
}

class TOp[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](val factory: GraphCoreCompanion[CC])
    extends RefSpec
    with Matchers
    with Examples[CC]
    with Visualizer[CC] {

  def `union ` {
    val expected =
      g union h should be(Expected.g_union_h)
  }
  def `difference ` {
    g diff h should be(Expected.g_diff_h)
  }
  def `intersection ` {
    val expected = factory(3 ~ 5, 4)
    given(g intersect h) { _ should be(expected) }
    given(g & h) { _ should be(expected) }
  }
}

class TImmutableOp extends RefSpec with Matchers with Examples[immutable.Graph] {
  val factory = immutable.Graph

  def `++ ` {
    g ++ h should be(Expected.g_union_h)
  }
  def `-- ` {
    g -- h should be(Expected.g_diff_h)
  }
}

class TMutableOp extends RefSpec with Matchers {

  val oEdgesG = List(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  val oEdgesH = List(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

  val (iFactory, mFactory) = (immutable.Graph, mutable.Graph)

  def initG = (iFactory(oEdgesG: _*), mFactory(oEdgesG: _*))
  def initH = (iFactory(oEdgesH: _*), mFactory(oEdgesH: _*))

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

  /* TODO
  def `intersection ` {
    val (iG, mG) = initG
    val (iH, mH) = initH
    val expected = mFactory(3 ~ 5, 4)
    (mG &= mH) should be(expected)
    (mG &= iH) should be(expected)
  }
 */
}
