package scalax.collection
package generator

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import org.scalatest.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.prop.PropertyChecks

import GraphEdge._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TGraphGenTest extends RefSpec with Matchers with PropertyChecks {

  final val minSuccessful = 5
  implicit val config     = PropertyCheckConfiguration(minSuccessful = minSuccessful, maxDiscardedFactor = 1.0)

  object `nr of minimum successful tests` {
    def `should be met` {
      var count = 0
      forAll { (i: Int) =>
        count += 1
      }
      count should be(minSuccessful)
    }
  }

  object `outer node set` {
    val order = 5
    implicit val arbitraryOuterNodes: Arbitrary[Set[Int]] =
      new GraphGen[Int, DiEdge, Graph](
        Graph,
        order,
        Gen.choose(0, 10 * order),
        NodeDegreeRange(1, 4),
        Set(DiEdge)
      ).outerNodeSet

    def `should conform to the passed size` {
      forAll(arbitrary[Set[Int]]) { (outerNodes: Set[Int]) =>
        outerNodes should have size (order)
      }
    }
  }

  type IntDiGraph = Graph[Int, DiEdge]

  def checkMetrics(g: IntDiGraph, metrics: GraphGen.Metrics[Int]) {
    import metrics._

    val degrees                 = g.degreeSeq
    val tolerableMaxExceed: Int = if (g.isHyper) 8 else 1

    g.order should be(order)
    g.isConnected should be(connected)

    degrees.min should be >= (nodeDegrees.min)
    degrees.max should be <= (nodeDegrees.max + tolerableMaxExceed)

    val totalDegree = g.totalDegree
    totalDegree should (be >= (expectedTotalDegree - maxDegreeDeviation) and
    be <= (expectedTotalDegree + maxDegreeDeviation))
  }

  object `tiny connected graph of [Int,DiEdge]` {
    implicit val arbitraryGraph = GraphGen.tinyConnectedIntDi[Graph](Graph)

    def `should conform to tiny metrics` {
      forAll(arbitrary[IntDiGraph]) { g: IntDiGraph =>
        checkMetrics(g, GraphGen.TinyInt)
      }
    }
  }

  object `small connected graph of [Int,DiEdge]` {
    implicit val arbitraryGraph = GraphGen.smallConnectedIntDi[Graph](Graph)

    def `should conform to small metrics` {
      forAll(arbitrary[IntDiGraph]) { g: IntDiGraph =>
        checkMetrics(g, GraphGen.SmallInt)
      }
    }
  }
}
