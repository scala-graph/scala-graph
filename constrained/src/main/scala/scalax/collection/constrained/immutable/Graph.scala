package scalax.collection.constrained
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.language.{higherKinds, postfixOps}
import scala.collection.Set
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

import scalax.collection.{Graph => CommonGraph}
import scalax.collection.GraphEdge.{EdgeCompanionBase, EdgeLike}
import scalax.collection.GraphPredef.{EdgeLikeIn, InParam, Param}
import scalax.collection.GraphTraversalImpl
import scalax.collection.mutable.ArraySet
import scalax.collection.generic.GraphCompanion
import scalax.collection.config.AdjacencyListArrayConfig

import generic.{GraphConstrainedCompanion, ImmutableGraphCompanion}
import config.ConstrainedConfig
import PreCheckFollowUp._

trait Graph[N, E[X] <: EdgeLikeIn[X]]
    extends scalax.collection.immutable.Graph[N, E]
    with scalax.collection.constrained.Graph[N, E]
    with GraphLike[N, E, Graph] {

  override def empty: Graph[N, E] = Graph.empty[N, E](edgeT, config)
}

object Graph extends ImmutableGraphCompanion[Graph] {

  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](edgeT, config)

  override protected[collection] def fromWithoutCheck[N, E[X] <: EdgeLikeIn[X]](
      nodes: Traversable[N],
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
    DefaultGraphImpl.fromWithoutCheck[N, E](nodes, edges)(edgeT, config)

  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(edgeT, config)

  // TODO: canBuildFrom
}

abstract class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]](iniNodes: Traversable[N] = Nil,
                                                          iniEdges: Traversable[E[N]] = Nil)(
    implicit override val edgeT: ClassTag[E[N]],
    override val config: DefaultGraphImpl.Config)
    extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
    with GraphTraversalImpl[N, E] {

  final override val graphCompanion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config

  @inline final protected def newNodeSet: NodeSetT = new NodeSet
  @transient protected[this] var _nodes: NodeSetT  = newNodeSet
  @inline final override def nodes                 = _nodes

  @transient protected[this] var _edges: EdgeSetT = new EdgeSet
  @inline final override def edges                = _edges

  initialize(iniNodes, iniEdges)

  @inline final override def empty: DefaultGraphImpl[N, E] = DefaultGraphImpl.empty(edgeT, config)

  @SerialVersionUID(8081L)
  final protected class NodeBase(value: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(value, hints)
      with InnerNodeTraversalImpl

  type NodeT = NodeBase
  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}

object DefaultGraphImpl extends ImmutableGraphCompanion[DefaultGraphImpl] {

  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config) =
    from(Set.empty[N], Set.empty[E[N]])(edgeT, config)

  override protected[collection] def fromWithoutCheck[N, E[X] <: EdgeLikeIn[X]](
      nodes: Traversable[N],
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
    new UserConstrainedGraphImpl[N, E](nodes, edges)(edgeT, config)

  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): DefaultGraphImpl[N, E] = {
    val existElems     = nodes.nonEmpty || edges.nonEmpty
    var preCheckResult = PreCheckResult(Abort)
    if (existElems) {
      val emptyGraph = empty[N, E](edgeT, config)
      val constraint = config.constraintCompanion[N, E, DefaultGraphImpl[N, E]](emptyGraph)
      preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) {
        constraint onAdditionRefused (nodes, edges, emptyGraph)
        return emptyGraph
      }
    }
    val newGraph = fromWithoutCheck[N, E](nodes, edges)(edgeT, config)
    if (existElems) {
      val emptyGraph = empty[N, E](edgeT, config)
      val constraint = config.constraintCompanion[N, E, DefaultGraphImpl[N, E]](emptyGraph)
      var handle     = false
      preCheckResult.followUp match {
        case Complete  =>
        case PostCheck => handle = !constraint.postAdd(newGraph, nodes, edges, preCheckResult)
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

@SerialVersionUID(7700L)
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]](iniNodes: Traversable[N] = Nil,
                                                         iniEdges: Traversable[E[N]] = Nil)(
    implicit override val edgeT: ClassTag[E[N]],
    override val config: DefaultGraphImpl.Config)
    extends DefaultGraphImpl[N, E](iniNodes, iniEdges)(edgeT, config)
    with UserConstrainedGraph[N, E, DefaultGraphImpl[N, E]] {

  final override val self              = this
  final override val constraintFactory = config.constraintCompanion
  final override val constraint        = constraintFactory(this)

  final override def copy(nodes: Traversable[N], edges: Traversable[E[N]]) =
    DefaultGraphImpl.from(nodes, edges)(edgeT, config)

  private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)

  private def readObject(in: ObjectInputStream): Unit = {
    _nodes = newNodeSet
    _edges = new EdgeSet
    initializeFrom(in, _nodes, _edges)
  }
}
