package scalax.collection
package generator

import scala.reflect.ClassTag

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scalax.collection.edges._
import scalax.collection.generic.{Edge, EdgeCompanionBase, GraphCompanion}
import scalax.collection.immutable.Graph
import scalax.collection.mutable.{Graph => MGraph}
/* TODO L
import scalax.collection.edges.labeled._
 */

class RandomGraphSpec extends RefSpec with Matchers {

  import RandomGraph._
  val normal = new IntFactory {
    val order       = 1000
    val nodeDegrees = NodeDegreeRange(4, 15)
  }

  /** Creates a `RandomGraph` generator that produces a graph
    *  with a constant order, constant `NodeDegreeRange` and a single edge type.
    */
  def generator[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, G]](
      edgeCompanion: EdgeCompanionBase,
      gCompanion: GraphCompanion[G],
      connected: Boolean
  )(implicit nodeTag: ClassTag[N], metrics: Metrics[N]) =
    new RandomGraph[N, E, G](
      gCompanion,
      metrics.order,
      metrics.nodeGen,
      metrics.nodeDegrees,
      Set(edgeCompanion),
      connected
    ) {
      val graphConfig = graphCompanion.defaultConfig
    }

  def checkOrder(g: AnyGraph[Int, DiEdge[Int]])(implicit metrics: Metrics[Int]): Unit =
    g.order should be(metrics.order)

  def checkSize(g: AnyGraph[Int, DiEdge[Int]])(implicit metrics: Metrics[Int]): Unit = {
    import metrics._
    val totalDegree = g.totalDegree
    val deviation   = totalDegree - expectedTotalDegree
    if (false)
      println(f"""  total degree=$totalDegree,
                 | isDense=$isDense,
                 | maxDev=$maxDegreeDeviation,
                 | deviation=$deviation,
                 | (${100f * deviation / totalDegree}%2.2f percent)""".stripMargin.linesIterator.mkString)
    totalDegree should (be >= (expectedTotalDegree - maxDegreeDeviation) and
      be <= (expectedTotalDegree + maxDegreeDeviation))
  }

  object `disconnected random graph` {
    implicit val metrics: Metrics[Int] = normal

    val g = generator[Int, DiEdge[Int], Graph](DiEdge, Graph, false).draw

    def `should have expected size`: Unit =
      checkOrder(g)
    checkSize(g)
  }

  object `connected random graph` {
    implicit val metrics: Metrics[Int] = normal

    val g = generator[Int, DiEdge[Int], MGraph](DiEdge, MGraph, true).draw

    def `should have expected size`: Unit =
      checkOrder(g)
    checkSize(g)
  }

  object `dense random graph` {
    implicit val dense: Metrics[Int] = new IntFactory {
      val order       = 100
      val nodeDegrees = NodeDegreeRange(55, 95)
    }
    val g = generator[Int, DiEdge[Int], Graph](DiEdge, Graph, true).draw

    def `should have dense metrics`: Unit =
      dense.isDense shouldBe true
    def `should have expected size`: Unit =
      checkOrder(g)
    checkSize(g)
  }

  /* TODO L
  object `default weighted random graph edges` {
    implicit val metrics: Metrics[Int] = RandomGraph.TinyInt
    val g                              = generator[Int, WDiEdge[Int], Graph](WDiEdge, Graph, true).draw

    def `should have distinct weights`: Unit = {
      val weights = MSet.empty[Long] ++ (g.edges map (_.weight))
      weights.size should be(g.size)
    }
  }

  object `default labeled random graph edges` {
    implicit val metrics: Metrics[Int] = RandomGraph.SmallInt
    val g                              = generator[Int, LDiEdge, Graph](LDiEdge, Graph, true).draw

    def `should have distinct labels`: Unit = {
      val labels = MSet.empty[Any] ++ (g.edges map (_.label))
      labels.size should be(g.size)
    }
  }
   */

  object IgnoreThis { // TODO
//  object `huge graph` {
    implicit val huge: Metrics[Int] = new IntFactory {
      val order       = 100000
      val nodeDegrees = normal.nodeDegrees
    }
    var g: MGraph[Int, DiEdge[Int]] = _

    def `should be fast enough`: Unit =
      g = generator[Int, DiEdge[Int], MGraph](DiEdge, MGraph, true).draw

    def `should have expected size`: Unit =
      checkOrder(g)
    // checkSize(g)
  }
}
