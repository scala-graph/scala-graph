package scalax.collection

import GraphPredef._, GraphEdge._

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec

class TEqualsTest extends RefSpec with Matchers {

  val oEdgesG              = List[UnDiEdge[Int]](1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  val oEdgesH              = List[UnDiEdge[Int]](3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)
  val (iFactory, mFactory) = (immutable.Graph, mutable.Graph)
  val none                 = Set.empty
  def initG                = (iFactory.from(none, oEdgesG), mFactory.from(none, oEdgesG))
  def initH                = (iFactory.from(none, oEdgesH), mFactory.from(none, oEdgesH))

  object `equals works properly` {
    def `over immutable and mutable graphs` {
      val (iG, mG) = initG
      val (iH, mH) = initH
      val ok       = iG == mG
      ok should be(true)
    }
  }
}
