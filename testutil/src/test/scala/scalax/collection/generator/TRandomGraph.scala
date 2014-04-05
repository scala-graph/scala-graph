package scalax.collection
package generator

import scala.language.higherKinds
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.util.Random

import org.scalatest.Matchers
import org.scalatest.{DoNotDiscover, Spec}

import GraphPredef._, GraphEdge._
import mutable.{Graph => MGraph}
import generic.GraphCompanion
import parameters.NodeDegreeRange

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TRandomGraphTest extends Spec with Matchers {

  import RandomGraph._
  val normal = new IntFactory {
    val order = 1000
    val nodeDegrees = NodeDegreeRange(4,15)
  }

  /** Creates a `RandomGraph` generator that produces a graph
   *  with a constant order, constant `NodeDegreeRange` and a single edge type.
   */
  def generator[N,
                E[X] <: EdgeLikeIn[X],
                G[X,Y[Z] <: EdgeLikeIn[Z]] <: Graph[X,Y] with GraphLike[X,Y,G]]
      (edgeCompanion: EdgeCompanionBase[E],
       gCompanion:    GraphCompanion[G],
       connected:     Boolean)
      (implicit edgeTag: TypeTag[E[N]],
       nodeTag: ClassTag[N],
       metrics: Metrics[N]) =
    new RandomGraph[N,E,G](
        gCompanion,
        metrics.order, 
        metrics.nodeGen, 
        metrics.nodeDegrees, 
        Set(edgeCompanion),
        connected) {
      val graphConfig = graphCompanion.defaultConfig
    }

  def checkOrder(g: Graph[Int,DiEdge])(implicit metrics: Metrics[Int]) {
    g.order should be (metrics.order)
  }
  def checkSize (g: Graph[Int,DiEdge])(implicit metrics: Metrics[Int]) {
    import metrics._
    val totalDegree = g.totalDegree
    val deviation = totalDegree - expectedTotalDegree
    if (false)
      println(f"""  total degree=$totalDegree,
                  | maxDev=$maxDegreeDeviation,
                  | deviation=$deviation,
                  | (${100f * deviation / totalDegree}%2.2f percent)""".
        stripMargin.lines.mkString)
    totalDegree should (be >= (expectedTotalDegree - maxDegreeDeviation) and
                        be <= (expectedTotalDegree + maxDegreeDeviation))
  }

  object `disconnected random graph` {
    implicit def metrics = normal
    val g = generator[Int,DiEdge, Graph](DiEdge,  Graph, false).draw
    
    def `should have expected size` {
      checkOrder(g)
      checkSize(g)
    }
  }
  object `connected random graph` {
    implicit def metrics = normal
    val g = generator[Int,DiEdge,MGraph](DiEdge, MGraph, true).draw

    def `should have expected size` {
      checkOrder(g)
      checkSize(g)
    }
  }
  object `dense random graph` {
    implicit val dense = new IntFactory {
      val order = 100
      val nodeDegrees = NodeDegreeRange(70, 100)
    }
    val g = generator[Int,DiEdge,Graph](DiEdge, Graph, true).draw

    def `should have expected size` {
      checkOrder(g)
      checkSize(g)
    }
  }
  object IgnoreThis {
//  object `huge graph` {
    implicit val huge = new IntFactory {
      val order = 100000
      val nodeDegrees = normal.nodeDegrees
    }
    var g: MGraph[Int,DiEdge] = _

    def `should be fast enough` {
      g = generator[Int,DiEdge,MGraph](DiEdge, MGraph, true).draw
    }
    
    def `should have expected size` {
      checkOrder(g)
      checkSize(g)
    }
  }
}