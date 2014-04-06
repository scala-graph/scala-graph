package scalax.collection
package generator

import scala.language.higherKinds
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.util.Buildable._

import GraphPredef.EdgeLikeIn, GraphEdge._
import generic.GraphCompanion

/** A `Graph` generator in terms of
 *  [[http://www.scalacheck.org/files/scalacheck_2.10-1.11.3-api/org/scalacheck/Gen$.html org.scalacheck.Gen]].
 *  
 * @tparam N Type of the nodes the generated will contain.
 * @tparam E Kind of type of the edges the generated will contain.
 * @tparam G Kind of type of the graph to be generated.
 * @param graphCompanion The graph companion the factory method `from`of which is
 *        to be called to create random graphs. 
 * @param order The total number of nodes the generated random graphs should contain.
 * @param nodeGen A node generator that is assumed to generate enough unique elements
 *                with respect to `order`. Typically it will be enough to define a range
 *                of elements ten fold of `order`. 
 * @param nodeDegree The characteristics of node degrees such as the span of degrees
 *        for the graph to be generated.
 * @param edgeCompanions The edge types to be used in the generated graph.
 * @param connected Whether the generated graph should be connected.
 * 
 * @author Peter Empen
 */
class GraphGen[N,
               E[N] <: EdgeLikeIn[N],
               G[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,G]](
    val graphCompanion: GraphCompanion[G],
    val order:          Int,
    nodeGen:            Gen[N],
    nodeDegrees:        NodeDegreeRange,
    edgeCompanions:     Set[EdgeCompanionBase[E]],
    connected:          Boolean = true)
   (implicit edgeTag: TypeTag[E[N]],
    nodeTag: ClassTag[N]) {

  protected[generator] def nodeSetGen: Gen[Set[N]] =
    Gen.containerOfN[Set,N](order, nodeGen) suchThat (_.size == order)
  
  protected[generator] val outerNodeSet = Arbitrary(nodeSetGen)
  
  private def ensureNodeGen: N = {
    var cnt = 0
    var node: Option[N] = None
    while (node.isEmpty && cnt < 100) {
      node = nodeGen.apply(Gen.Parameters.default)
      cnt += 1
    }
    node getOrElse (throw new IllegalArgumentException(
        "The supplied node generator fails to generate valid values. Try to ease its filter condition."))
  } 

  private def generator =
    new RandomGraph[N,E,G](
        graphCompanion,
        order, 
        ensureNodeGen, 
        nodeDegrees, 
        edgeCompanions,
        connected) {
      val graphConfig = graphCompanion.defaultConfig
    }
  
  def apply: Gen[G[N,E]] = Gen.const(0) map ( _ => generator.draw)  
}

/** Provides templates for arbitrary graph metrics including shortcuts for metrics
 *  of small arbitrary graphs with a node type of `Int`.  
 */
object GraphGen {
  
  trait Metrics[N] extends RandomGraph.MetricsBase[N] {
    def nodeGen: Gen[N]
  }
  object TinyInt extends Metrics[Int] {
    val order = 5
    val nodeGen = Gen.choose(1, minMax)
    val nodeDegrees = NodeDegreeRange(2,4)
  }
  object SmallInt extends Metrics[Int] {
    val order = 20
    val nodeGen = Gen.choose(1, minMax)
    val nodeDegrees = NodeDegreeRange(2,5)
  }

  def tinyConnectedIntDi[G[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,G]]
      (companion: GraphCompanion[G]): Arbitrary[G[Int,DiEdge]] =
    Arbitrary {
      import TinyInt._
      new GraphGen[Int,DiEdge,G](
          companion, order, nodeGen, nodeDegrees, Set(DiEdge), connected).apply
  }
  
  def smallConnectedIntDi[G[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,G]]
      (companion: GraphCompanion[G]): Arbitrary[G[Int,DiEdge]] =
    Arbitrary {
      import SmallInt._
      new GraphGen[Int,DiEdge,G](
          companion, order, nodeGen, nodeDegrees, Set(DiEdge), connected).apply
  }
}