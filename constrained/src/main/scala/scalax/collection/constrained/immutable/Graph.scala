package scalax.collection.constrained
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.language.higherKinds
import scala.collection.Set
import scala.reflect.ClassTag

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.GraphTraversalImpl
import scalax.collection.mutable.ArraySet

import generic.ImmutableGraphCompanion
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
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): Graph[N, E] =
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
  @inline final override def nodes: NodeSet        = _nodes

  @transient protected[this] var _edges: EdgeSetT = new EdgeSet
  @inline final override def edges: EdgeSet       = _edges

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

  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
    fromWithoutCheck(Set.empty, Set.empty)(edgeT, config)

  override protected[collection] def fromWithoutCheck[N, E[X] <: EdgeLikeIn[X]](
      nodes: Traversable[N],
      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
    new UserConstrainedGraphImpl[N, E](nodes, edges)(edgeT, config)

  final override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): DefaultGraphImpl[N, E] = from_?(nodes, edges) getOrElse empty[N, E](edgeT, config)

  def from_?[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N], edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): Either[ConstraintViolation, DefaultGraphImpl[N, E]] = {
    def emptyGraph = empty[N, E](edgeT, config)
    if (nodes.isEmpty && edges.isEmpty) Right(emptyGraph)
    else {
      val constraint     = config.constraintCompanion[N, E, DefaultGraphImpl[N, E]](emptyGraph)
      val preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) Left(preCheckResult)
      else {
        val newGraph = fromWithoutCheck[N, E](nodes, edges)(edgeT, config)
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
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]](iniNodes: Traversable[N] = Nil,
                                                         iniEdges: Traversable[E[N]] = Nil)(
    implicit override val edgeT: ClassTag[E[N]],
    override val config: DefaultGraphImpl.Config)
    extends DefaultGraphImpl[N, E](iniNodes, iniEdges)(edgeT, config)
    with UserConstrainedGraph[N, E, DefaultGraphImpl[N, E]] {

  final override val self              = this
  final override val constraintFactory = config.constraintCompanion
  final override val constraint        = constraintFactory(this)

  final protected def copy(nodes: Traversable[N], edges: Traversable[E[N]]): DefaultGraphImpl[N, E] =
    copy_?(nodes, edges) getOrElse empty

  def copy_?(nodes: Traversable[N], edges: Traversable[E[N]]): Either[ConstraintViolation, DefaultGraphImpl[N, E]] =
    DefaultGraphImpl.from_?(nodes, edges)(edgeT, config)

  private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)

  private def readObject(in: ObjectInputStream): Unit = {
    _nodes = newNodeSet
    _edges = new EdgeSet
    initializeFrom(in, _nodes, _edges)
  }
}
