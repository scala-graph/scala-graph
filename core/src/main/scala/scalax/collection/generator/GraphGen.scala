package scalax.collection
package generator

import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.rng.Seed
import org.scalacheck.util.Buildable._

import scalax.collection.generic._
import scalax.collection.edges._
import scalax.collection.generic.GraphCompanion

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
  * @param nodeDegrees The characteristics of node degrees such as the span of degrees
  *        for the graph to be generated.
  * @param edgeCompanions The edge types to be used in the generated graph.
  * @param connected Whether the generated graph should be connected.
  *
  * @author Peter Empen
  */
class GraphGen[N, E <: Edge[N], G[X, Y <: Edge[X]] <: Graph[X, Y] with GraphLike[X, Y, G]](
    val graphCompanion: GraphCompanion[G],
    val order: Int,
    nodeGen: Gen[N],
    nodeDegrees: NodeDegreeRange,
    edgeCompanions: Set[EdgeCompanionBase],
    connected: Boolean = true,
    weightFactory: Option[Gen[Long]] = None,
    labelFactory: Option[Gen[Any]] = None
)(implicit nodeTag: ClassTag[N]) {

  protected[generator] def nodeSetGen: Gen[Set[N]] =
    Gen.containerOfN[Set, N](order, nodeGen) suchThat (_.size == order)

  protected[generator] val outerNodeSet = Arbitrary(nodeSetGen)

  final private class NonFailing[N](gen: Gen[N], genName: String) {
    def draw: N = {
      var cnt              = 0
      var value: Option[N] = None
      while (value.isEmpty && cnt < 100) {
        value = gen.apply(Gen.Parameters.default, Seed.random())
        cnt += 1
      }
      value getOrElse (throw new IllegalArgumentException(
        s"$genName generator fails to generate enough valid values. Try to ease its filter condition."
      ))
    }
  }

  private def generator = {
    val nonFailingWeights = weightFactory map (new NonFailing(_, "weight"))
    val nonFailingLabels  = labelFactory map (new NonFailing(_, "label"))
    new RandomGraph[N, E, G](
      graphCompanion,
      order,
      new NonFailing(nodeGen, "node").draw,
      nodeDegrees,
      edgeCompanions,
      connected,
      weightFactory map (f => () => nonFailingWeights.get.draw),
      labelFactory map (f => () => nonFailingLabels.get.draw)
    ) {
      val graphConfig = graphCompanion.defaultConfig
    }
  }

  def apply: Gen[G[N, E]] = Gen.const(0) map (_ => generator.draw)
}

/** Provides convenience metrics and methods for the generation of graphs
  *  by means of `org.scalacheck.Arbitrary[G[N,E]]`.
  *
  * @define COMPANION The graph companion object such as `scalax.collection.Graph`
  *         to be used to generate graphs.
  * @define METRICS The `Metrics` to be applied to the generated graph.
  */
object GraphGen {

  def apply[N, E <: Edge[N], G[X, Y <: Edge[X]] <: Graph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GraphCompanion[G],
      order: Int,
      nodeGen: Gen[N],
      nodeDegrees: NodeDegreeRange,
      edgeCompanions: Set[EdgeCompanionBase],
      connected: Boolean,
      weightFactory: Option[Gen[Long]],
      labelFactory: Option[Gen[Any]]
  )(implicit nodeTag: ClassTag[N]): GraphGen[N, E, G] =
    new GraphGen[N, E, G](
      graphCompanion,
      order,
      nodeGen,
      nodeDegrees,
      edgeCompanions,
      connected,
      weightFactory,
      labelFactory
    )

  def fromMetrics[N, E <: Edge[N], G[X, Y <: Edge[X]] <: Graph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GraphCompanion[G],
      metrics: Metrics[N],
      edgeCompanions: Set[EdgeCompanionBase]
  )(implicit nodeTag: ClassTag[N]): GraphGen[N, E, G] =
    new GraphGen[N, E, G](
      graphCompanion,
      metrics.order,
      metrics.nodeGen,
      metrics.nodeDegrees,
      edgeCompanions,
      metrics.connected
    )

  /** Represents graph metrics like `order`, `nodeDegrees` and `connected`
    *  excluding the type of nodes, edges and the type of the graph to be generated.
    */
  trait Metrics[N] extends RandomGraph.MetricsBase[N] {
    def nodeGen: Gen[N]
  }

  /** Predefined metrics of a 'tiny' graph with the node type of `Int`,
    *  an order of `5` and a node degree range of `2` to `4`
    *  including `org.scalacheck.Gen[Int]`.
    */
  object TinyInt extends Metrics[Int] {
    val order       = 5
    val nodeGen     = Gen.choose(1, minMax)
    val nodeDegrees = NodeDegreeRange(2, 4)
  }

  /** Predefined metrics of a 'small' graph with the node type of `Int`,
    *  an order of `20` and a node degree range of `2` to `5`
    *  including `org.scalacheck.Gen[Int]`.
    */
  object SmallInt extends Metrics[Int] {
    val order       = 20
    val nodeGen     = Gen.choose(1, minMax)
    val nodeDegrees = NodeDegreeRange(2, 5)
  }

  /** Returns an `org.scalacheck.Arbitrary[G[Int,DiEdge]]` for tiny, connected,
    *  non-labeled directed graphs with the metrics defined by `TinyInt`.
    *
    *  @param graphCompanion $COMPANION
    */
  def tinyConnectedIntDi[G[X, Y <: Edge[X]] <: Graph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GraphCompanion[G]
  ): Arbitrary[G[Int, DiEdge[Int]]] =
    diGraph[Int, G](graphCompanion, TinyInt)

  /** Returns an `org.scalacheck.Arbitrary[G[Int,DiEdge]]` for small, connected,
    *  non-labeled directed graphs with the metrics defined by `SmallInt`.
    *
    * @param graphCompanion $COMPANION
    */
  def smallConnectedIntDi[G[X, Y <: Edge[X]] <: Graph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GraphCompanion[G]
  ): Arbitrary[G[Int, DiEdge[Int]]] =
    diGraph[Int, G](graphCompanion, SmallInt)

  /** Returns an `org.scalacheck.Arbitrary[G[N,DiEdge]]` for non-labeled directed graphs
    *  of any metrics and any type.
    *
    * @param graphCompanion $COMPANION
    * @param metrics $METRICS
    */
  def diGraph[N, G[X, Y <: Edge[X]] <: Graph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GraphCompanion[G],
      metrics: Metrics[N]
  )(implicit nodeTag: ClassTag[N]): Arbitrary[G[N, DiEdge[N]]] =
    Arbitrary {
      GraphGen.fromMetrics[N, DiEdge[N], G](graphCompanion, metrics, Set(DiEdge)).apply
    }

  /** Returns an `org.scalacheck.Arbitrary[G[N,UnDiEdge]]` for non-labeled undirected graphs
    *  of any metrics and any type.
    *
    * @param graphCompanion $COMPANION
    * @param metrics $METRICS
    */
  def unDiGraph[N, G[X, Y <: Edge[X]] <: Graph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GraphCompanion[G],
      metrics: Metrics[N]
  )(implicit nodeTag: ClassTag[N]): Arbitrary[G[N, UnDiEdge[N]]] =
    Arbitrary {
      GraphGen.fromMetrics[N, UnDiEdge[N], G](graphCompanion, metrics, Set(UnDiEdge)).apply
    }
}
