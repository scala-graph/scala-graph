package scalax.collection
package immutable

import language.higherKinds
import collection.Set
import collection.generic.CanBuildFrom
import scala.reflect.runtime.universe._

import scalax.collection.{Graph => CommonGraph}
import GraphEdge.{EdgeLike, EdgeCompanionBase, UnDiEdge}
import GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn} 
import generic.{GraphCompanion, ImmutableGraphCompanion, MutableGraphCompanion}
import config.AdjacencyListArrayConfig
import mutable.{GraphBuilder, ArraySet}
import io._

trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	CommonGraph[N,E]
	with	  GraphLike[N, E, Graph] 
{
	override def empty: Graph[N,E] = Graph.empty[N,E]
}
object Graph extends ImmutableGraphCompanion[Graph]
{
	def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: TypeTag[E[N]],
                                      config: Config = defaultConfig): Graph[N,E] =
	  DefaultGraphImpl.empty[N,E](edgeT, config)
  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N] = Seq.empty[N],
                                              edges: Iterable[E[N]])
                                             (implicit edgeT: TypeTag[E[N]],
                                              config: Config = defaultConfig) : Graph[N,E] =
    DefaultGraphImpl.from[N,E](nodes, edges)(
                               edgeT, config)
  override def fromStream[N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]])
     (implicit edgeT: TypeTag[E[N]],
      config: Config = defaultConfig) : Graph[N,E] =
    DefaultGraphImpl.fromStream[N,E](nodeStreams, nodes, edgeStreams, edges)(
                                     edgeT, config)
                                     
  implicit def cbfUnDi[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: TypeTag[E[N]],
                                                 config: Config = defaultConfig) =
    new GraphCanBuildFrom[N,E]()(edgeT, config).asInstanceOf[
      GraphCanBuildFrom[N,E]
      with CanBuildFrom[Graph[_,UnDiEdge], GraphParam[N,E], Graph[N,E]]]
}
@SerialVersionUID(71L)
class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
    ( iniNodes: Iterable[N]    = Set[N](),
      iniEdges: Iterable[E[N]] = Set[E[N]]() )
    ( implicit @transient override val edgeT: TypeTag[E[N]],
      override val config: DefaultGraphImpl.Config with AdjacencyListArrayConfig)
  extends Graph[N,E]
  with    AdjacencyListGraph[N,E,DefaultGraphImpl]
  with    GraphTraversalImpl[N,E]
{
  @transient override final val graphCompanion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config

  @inline final def newNodeSet: NodeSetT = new NodeSet
  override final val nodes = newNodeSet
  override final val edges = new EdgeSet
  initialize(iniNodes, iniEdges)

  def this(nodeStreams: Iterable[NodeInputStream[N]],
           nodes:       Iterable[N],
           edgeStreams: Iterable[GenEdgeInputStream[N,E]],
           edges:       Iterable[E[N]])
          (implicit edgeT: TypeTag[E[N]],
           config: DefaultGraphImpl.Config) = {
    this()
    from(nodeStreams, nodes, edgeStreams, edges)
  }
  override protected[this] def newBuilder =
    new GraphBuilder[N,E,DefaultGraphImpl](DefaultGraphImpl)
  @inline final override def empty: DefaultGraphImpl[N,E] =
    DefaultGraphImpl.empty[N,E]
  @inline final override def copy(nodes: Iterable[N],
                                  edges: Iterable[E[N]])=
    DefaultGraphImpl.from[N,E](nodes, edges)

  @SerialVersionUID(7170L)
  final protected class NodeBase(value: N, hints: ArraySet.Hints)
    extends InnerNodeImpl(value, hints)
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}
object DefaultGraphImpl extends ImmutableGraphCompanion[DefaultGraphImpl]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: TypeTag[E[N]],
                                               config: Config = defaultConfig) =
    new DefaultGraphImpl[N,E]()(edgeT, config)
  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N] = Seq.empty[N],
                                              edges: Iterable[E[N]])
                                             (implicit edgeT: TypeTag[E[N]],
                                              config: Config = defaultConfig) =
    new DefaultGraphImpl[N,E](nodes, edges)(
                              edgeT, config)
  override def fromStream[N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]])
     (implicit edgeT: TypeTag[E[N]],
      config: Config = defaultConfig): DefaultGraphImpl[N,E] =
    new DefaultGraphImpl[N,E](nodeStreams, nodes, edgeStreams, edges)(
                              edgeT, config)
  implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: TypeTag[E[N]],
                                                      config: Config = defaultConfig)
      : GraphCanBuildFrom[N,E] =
    new GraphCanBuildFrom[N,E]()(edgeT, config)
}
