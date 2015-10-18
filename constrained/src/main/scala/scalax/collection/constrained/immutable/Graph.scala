package scalax.collection.constrained
package immutable

import scala.language.{higherKinds, postfixOps}
import scala.collection.{Set, Iterable}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

import scalax.collection.{Graph => CommonGraph}
import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.GraphPredef.{EdgeLikeIn, Param, InParam}
import scalax.collection.GraphTraversalImpl
import scalax.collection.mutable.ArraySet
import scalax.collection.generic.GraphCompanion
import scalax.collection.config.AdjacencyListArrayConfig

import generic.{GraphConstrainedCompanion, ImmutableGraphCompanion}
import config.ConstrainedConfig
import PreCheckFollowUp._
    
trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	scalax.collection.immutable.Graph[N,E]
	with    scalax.collection.constrained.Graph[N,E]
	with	  GraphLike[N, E, Graph] 
{
	override def empty: Graph[N,E] = Graph.empty[N,E](edgeT, config)
}
object Graph extends ImmutableGraphCompanion[Graph]
{
	override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                               config: Config): Graph[N,E] =
	  DefaultGraphImpl.empty[N,E](edgeT, config)

  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]]
	   (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeT: ClassTag[E[N]],
      config: Config): DefaultGraphImpl[N,E] =
    DefaultGraphImpl.fromUnchecked[N,E](nodes, edges)(edgeT, config)

  override def from [N, E[X] <: EdgeLikeIn[X]]
     (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeT: ClassTag[E[N]],
      config: Config): Graph[N,E] =
    DefaultGraphImpl.from[N,E](nodes, edges)(edgeT, config)

  // TODO: canBuildFrom
}
abstract class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
   (iniNodes: Iterable[N]    = Set.empty[N],
    iniEdges: Iterable[E[N]] = Set.empty[E[N]])
   (implicit override val edgeT: ClassTag[E[N]],
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
    DefaultGraphImpl.empty(edgeT, config)
  @inline final override def clone: DefaultGraphImpl[N,E] = {
    DefaultGraphImpl.fromUnchecked(nodes.toOuter, edges.toOuter)(
                                   edgeT, config)
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
  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                               config: Config) =
    from(Set.empty[N], Set.empty[E[N]])(edgeT, config)

  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                              edges: Iterable[E[N]])
                                             (implicit edgeT: ClassTag[E[N]],
                                              config: Config): DefaultGraphImpl[N,E] =
    new UserConstrainedGraphImpl[N,E](nodes, edges)(edgeT, config)

  override def from [N, E[X] <: EdgeLikeIn[X]]
     (nodes: Iterable[N],
      edges: Iterable[E[N]])
     (implicit edgeT: ClassTag[E[N]],
      config: Config) : DefaultGraphImpl[N,E] =
  { val existElems = nodes.nonEmpty || edges.nonEmpty
    var preCheckResult = PreCheckResult(Abort)
    if (existElems) {
      val emptyGraph = empty[N,E](edgeT, config)
      val constraint = config.constraintCompanion(emptyGraph)
      preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (nodes, edges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = fromUnchecked[N,E](nodes, edges)(edgeT, config)
    if (existElems) {
      val emptyGraph = empty[N,E](edgeT, config)
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
  // TODO: canBuildFrom
}
@SerialVersionUID(-73L)
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]]
   (iniNodes: Iterable[N]    = Set.empty[N],
    iniEdges: Iterable[E[N]] = Set.empty[E[N]])
   (implicit override val edgeT: ClassTag[E[N]],
    override val config: DefaultGraphImpl.Config)
  extends DefaultGraphImpl    [N,E](iniNodes, iniEdges)(edgeT, config)
  with    UserConstrainedGraph[N,E]
{
  final override val self = this
  final override val constraintFactory = config.constraintCompanion
  final override val constraint  = constraintFactory(this)
  final override def copy(nodes: Iterable[N],
                          edges: Iterable[E[N]]) =
    DefaultGraphImpl.from(nodes, edges)(edgeT, config)
}
