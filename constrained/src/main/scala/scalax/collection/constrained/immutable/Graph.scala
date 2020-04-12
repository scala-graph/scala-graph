package scalax.collection.constrained
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.Set

import scalax.collection.GraphPredef.EdgeLike
import scalax.collection.GraphTraversalImpl
import scalax.collection.mutable.ArraySet

import generic.ImmutableGraphCompanion
import PreCheckFollowUp._

trait Graph[N, E <: EdgeLike[N]]
    extends scalax.collection.immutable.Graph[N, E]
    with scalax.collection.constrained.Graph[N, E]
    with GraphLike[N, E, Graph] {

  override def empty: Graph[N, E] = Graph.empty[N, E](config)
}

object Graph extends ImmutableGraphCompanion[Graph] {

  override def empty[N, E <: EdgeLike[N]]( config: Config): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](config)

  override protected[collection] def fromWithoutCheck[N, E <: EdgeLike[N]](
      nodes: Iterable[N],
      edges: Iterable[E])( config: Config): Graph[N, E] =
    DefaultGraphImpl.fromWithoutCheck[N, E](nodes, edges)(config)

  override def from[N, E <: EdgeLike[N]](nodes: Iterable[N], edges: Iterable[E])(

      config: Config): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(config)

  // TODO: canBuildFrom
}

abstract class DefaultGraphImpl[N, E <: EdgeLike[N]](iniNodes: Iterable[N] = Nil, iniEdges: Iterable[E] = Nil)(

    override val config: DefaultGraphImpl.Config)
    extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
    with GraphTraversalImpl[N, E] {

  final override val companion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config

  @inline final protected def newNodeSet: NodeSetT = new NodeSet
  @transient protected[this] var _nodes: NodeSetT  = newNodeSet
  @inline final override def nodes: NodeSet        = _nodes

  @transient protected[this] var _edges: EdgeSetT = new EdgeSet
  @inline final override def edges: EdgeSet       = _edges

  initialize(iniNodes, iniEdges)

  @inline final override def empty: DefaultGraphImpl[N, E] = DefaultGraphImpl.empty(config)

  @SerialVersionUID(8081L)
  final protected class NodeBase(override val outer: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(outer, hints)
      with InnerNodeTraversalImpl

  type NodeT = NodeBase
  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}

object DefaultGraphImpl extends ImmutableGraphCompanion[DefaultGraphImpl] {

  override def empty[N, E <: EdgeLike[N]](
                                                config: Config): DefaultGraphImpl[N, E] =
    fromWithoutCheck(Set.empty, Set.empty)(config)

  override protected[collection] def fromWithoutCheck[N, E[+X] <: EdgeLikeIn[X]](
      nodes: Iterable[N],
      edges: Iterable[E])( config: Config): DefaultGraphImpl[N, E] =
    new UserConstrainedGraphImpl[N, E](nodes, edges)(config)

  final override def from[N, E <: EdgeLike[N]](nodes: Iterable[N], edges: Iterable[E])(

      config: Config): DefaultGraphImpl[N, E] = from_?(nodes, edges) getOrElse empty[N, E](config)

  def from_?[N, E <: EdgeLike[N]](nodes: Iterable[N], edges: Iterable[E])(

      config: Config): Either[ConstraintViolation, DefaultGraphImpl[N, E]] = {
    def emptyGraph = empty[N, E](config)
    if (nodes.isEmpty && edges.isEmpty) Right(emptyGraph)
    else {
      val constraint     = config.constraintCompanion[N, E, DefaultGraphImpl[N, E]](emptyGraph)
      val preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) Left(preCheckResult)
      else {
        val newGraph = fromWithoutCheck[N, E](nodes, edges)(config)
        preCheckResult.followUp match {
          case Complete  => Right(newGraph)
          case PostCheck => constraint.postAdd(newGraph, nodes, edges, preCheckResult)
          case Abort     => Left(preCheckResult)
        }
      }
    }
  }
}

@SerialVersionUID(7700L)
class UserConstrainedGraphImpl[N, E <: EdgeLike[N]](iniNodes: Iterable[N] = Nil, iniEdges: Iterable[E[N]] = Nil)(

    override val config: DefaultGraphImpl.Config)
    extends DefaultGraphImpl[N, E](iniNodes, iniEdges)(config)
    with UserConstrainedGraph[N, E, DefaultGraphImpl[N, E]] {

  final override val self              = this
  final override val constraintFactory = config.constraintCompanion
  final override val constraint        = constraintFactory(this)

  final protected def copy(nodes: Iterable[N], edges: Iterable[E]): DefaultGraphImpl[N, E] =
    copy_?(nodes, edges) getOrElse empty

  def copy_?(nodes: Iterable[N], edges: Iterable[E]): Either[ConstraintViolation, DefaultGraphImpl[N, E]] =
    DefaultGraphImpl.from_?(nodes, edges)(config)

  private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)

  private def readObject(in: ObjectInputStream): Unit = {
    _nodes = newNodeSet
    _edges = new EdgeSet
    initializeFrom(in, _nodes, _edges)
  }
}
