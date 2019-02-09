package scalax.collection

import language.{higherKinds, postfixOps}

import GraphPredef._

import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TEqualsTest extends RefSpec with Matchers {

  val oEdgesG = List(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  val oEdgesH = List(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

  val (iFactory, mFactory) = (immutable.Graph, mutable.Graph)

  def initG = (iFactory(oEdgesG: _*), mFactory(oEdgesG: _*))
  def initH = (iFactory(oEdgesH: _*), mFactory(oEdgesH: _*))

  object `equals works properly` {
    def `over immutable and mutable graphs` {
      val (iG, mG) = initG
      iG should === (mG)

      val (iH, mH) = initH
      iH should === (mH)
    }
  }
}
