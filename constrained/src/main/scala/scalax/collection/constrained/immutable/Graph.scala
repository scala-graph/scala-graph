package scalax.collection.constrained
package immutable

import collection.{Set, Iterable}
import collection.generic.CanBuildFrom
import scalax.collection.{Graph => CommonGraph}
import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn}
import scalax.collection.GraphTraversalImpl
import scalax.collection.mutable.ArraySet
import scalax.collection.generic.GraphCompanion
import scalax.collection.config.AdjacencyListArrayConfig
import scalax.collection.io._

import generic.{GraphConstrainedCompanion, ImmutableGraphCompanion}
import config.ConstrainedConfig
import PreCheckFollowUp._
    
trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	scalax.collection.immutable.Graph[N,E]
	with    scalax.collection.constrained.Graph[N,E]
	with	  GraphLike[N, E, Graph] 
{
	override def empty: Graph[N,E] = Graph.empty[N,E](edgeManifest, config)
}
object Graph extends ImmutableGraphCompanion[Graph]
{
	override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeManifest: Manifest[E[N]],
                                               config: Config): Graph[N,E] =
	  DefaultGraphImpl.empty[N,E](edgeManifest, config)

  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]]
	   (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config): DefaultGraphImpl[N,E] =
    DefaultGraphImpl.fromUnchecked[N,E](nodes, edges)(edgeManifest, config)

  override def from [N, E[X] <: EdgeLikeIn[X]]
     (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config): Graph[N,E] =
    DefaultGraphImpl.from[N,E](nodes, edges)(edgeManifest, config)

  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config): Graph[N,E] =
    DefaultGraphImpl.fromStream[N,E](nodeStreams, nodes, edgeStreams, edges)(
                                     edgeManifest, config)
  // TODO: canBuildFrom
}
abstract class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
   (iniNodes: Iterable[N]    = Set.empty[N],
    iniEdges: Iterable[E[N]] = Set.empty[E[N]])
   (implicit override val edgeManifest: Manifest[E[N]],
    override val config: DefaultGraphImpl.Config)
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

  @inline final override def empty: DefaultGraphImpl[N,E] =
    DefaultGraphImpl.empty(edgeManifest, config)
  @inline final override def clone: DefaultGraphImpl[N,E] = {
    DefaultGraphImpl.fromUnchecked(nodes.toNodeInSet, edges.toEdgeInSet)(
                                   edgeManifest, config)
  }
  @SerialVersionUID(8081L)
  final protected class NodeBase(value: N, hints: ArraySet.Hints)
    extends InnerNodeImpl(value, hints)
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}
object DefaultGraphImpl extends ImmutableGraphCompanion[DefaultGraphImpl]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeManifest: Manifest[E[N]],
                                               config: Config) =
    from(Set.empty[N], Set.empty[E[N]])(edgeManifest, config)

  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                              edges: Iterable[E[N]])
                                             (implicit edgeManifest: Manifest[E[N]],
                                              config: Config): DefaultGraphImpl[N,E] =
    new UserConstrainedGraphImpl[N,E](nodes, edges)(edgeManifest, config)

  override def from [N, E[X] <: EdgeLikeIn[X]]
     (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config) : DefaultGraphImpl[N,E] =
  { val existElems = nodes.nonEmpty || edges.nonEmpty
    var preCheckResult = PreCheckResult(Abort)
    if (existElems) {
      val emptyGraph = empty[N,E](edgeManifest, config)
      val constraint = config.constraintCompanion(emptyGraph)
      preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (nodes, edges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = fromUnchecked[N,E](nodes, edges)(edgeManifest, config)
    if (existElems) {
      val emptyGraph = empty[N,E](edgeManifest, config)
      val constraint = config.constraintCompanion(emptyGraph)
      var handle = false
      preCheckResult.followUp match {
        case Complete  =>
        case PostCheck => handle = ! constraint.postAdd(newGraph, nodes, edges, preCheckResult)
        case Abort     => handle = true
      }
      if (handle) {
        constraint.onAdditionRefused(nodes, edges, newGraph)
        emptyGraph
      } else
        newGraph
    } else
      newGraph
  }
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      iniNodes:    Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      iniEdges:    Iterable[E[N]]               = Seq.empty[E[N]])
     (implicit edgeManifest: Manifest[E[N]],
      config: Config) : DefaultGraphImpl[N,E] =
  {
    var preCheckResult = PreCheckResult(Abort)
    if (iniNodes.nonEmpty || iniEdges.nonEmpty) {
      val emptyGraph = empty[N,E](edgeManifest, config)
      val constraint = config.constraintCompanion(emptyGraph)
      preCheckResult = constraint.preCreate(iniNodes, iniEdges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (iniNodes, iniEdges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = new UserConstrainedGraphImpl[N,E]()(edgeManifest, config) {
      from(nodeStreams, iniNodes, edgeStreams, iniEdges)
    }
    var handle = false
    preCheckResult.followUp match {
      case Complete  =>
      case PostCheck => handle = ! newGraph.postAdd(newGraph, iniNodes, iniEdges, preCheckResult)
      case Abort     => handle = true
    }
    if (handle) {
      newGraph.onAdditionRefused(iniNodes, iniEdges, newGraph)
      empty[N,E](edgeManifest, config)
    } else
      newGraph
  }
  // TODO: canBuildFrom
}
@SerialVersionUID(-73L)
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]]
   (iniNodes: Iterable[N]    = Set.empty[N],
    iniEdges: Iterable[E[N]] = Set.empty[E[N]])
   (implicit override val edgeManifest: Manifest[E[N]],
    override val config: DefaultGraphImpl.Config)
  extends DefaultGraphImpl    [N,E](iniNodes, iniEdges)(edgeManifest, config)
  with    UserConstrainedGraph[N,E]
{
  final override val self = this
  final override val constraintFactory = config.constraintCompanion
  final override val constraint  = constraintFactory(this)
  final override def copy(nodes: Iterable[N],
                          edges: Iterable[E[N]]) =
    DefaultGraphImpl.from(nodes, edges)(edgeManifest, config)
}
