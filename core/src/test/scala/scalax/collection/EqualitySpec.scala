package scalax.collection

import org.scalatest.matchers.should.Matchers

import org.scalatest.Suites
import org.scalatest.refspec.RefSpec

import scalax.collection.GraphEdge.{DiEdge, EdgeLike}
import scalax.collection.GraphPredef._
import scalax.collection.generic.GraphCoreCompanion

class EqualitySpec
    extends Suites(
      new Equality[immutable.Graph](immutable.Graph),
      new Equality[mutable.Graph](mutable.Graph),
      new EqualityMixed
    )

private class Equality[CC[N, E <: EdgeLike[N]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC]
) extends RefSpec
    with Matchers {

  private val seq_1_3   = Seq(1, 3)
  private val gInt_1_3  = factory(seq_1_3.toOuterElems[DiEdge[Int]]: _*)
  private val gString_A = factory("A")

  def `Eq ` : Unit = {
    factory[Int, Nothing]() shouldEqual factory[Int, DiEdge]()
    gInt_1_3 shouldEqual factory(1, 3)
    gString_A shouldEqual factory("A")

    factory() shouldNot be(factory(1))
    gInt_1_3 shouldNot be(factory(2, 3))
    gString_A shouldNot be(factory("B"))

    gInt_1_3 shouldEqual (immutable.Graph(1) + 3)
  }
}

private class EqualityMixed extends RefSpec with Matchers {

  val oEdgesG = List(1 ~ 2, 2 ~ 3, 2 ~ 4, 3 ~ 5, 4 ~ 5)
  val oEdgesH = List(3 ~ 4, 3 ~ 5, 4 ~ 6, 5 ~ 6)

  val (iFactory, mFactory) = (immutable.Graph, mutable.Graph)

  def initG = (iFactory(oEdgesG: _*), mFactory(oEdgesG: _*))
  def initH = (iFactory(oEdgesH: _*), mFactory(oEdgesH: _*))

  object `equals works properly` {
    def `over immutable and mutable graphs`: Unit = {
      val (iG, mG) = initG
      iG should ===(mG)

      val (iH, mH) = initH
      iH should ===(mH)
    }
  }
}
