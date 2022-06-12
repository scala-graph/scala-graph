package scalax.collection.constrained
package mutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.Set
import scala.reflect.ClassTag

import scalax.collection.GraphTraversalImpl
import scalax.collection.GraphPredef.Edge
import scalax.collection.mutable.{ArraySet, BuilderImpl}
import scalax.collection.config.AdjacencyListArrayConfig
import scalax.collection.constrained.{Graph => CGraph, GraphLike => CGraphLike}
import PreCheckFollowUp._
import generic.GraphConstrainedCompanion
import config.GenConstrainedConfig

class GraphBuilder[N, E <: Edge[N], GC[N, E <: Edge[N]] <: CGraph[N, E] with CGraphLike[N, E, GC]](
    companion: GraphConstrainedCompanion[GC]
)(config: GenConstrainedConfig)
    extends BuilderImpl[N, E, GC] {
  def result: This =
    companion.from(nodes, edges)(config.asInstanceOf[companion.Config])
}

import scalax.collection.constrained.generic.MutableGraphCompanion

trait Graph[N, E <: Edge[N]]
    extends scalax.collection.mutable.Graph[N, E]
    with scalax.collection.constrained.Graph[N, E]
    with GraphLike[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E](config)
}

object Graph extends MutableGraphCompanion[Graph] {
  override def empty[N, E <: Edge[N]](config: Config): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](config)

  override protected[collection] def fromWithoutCheck[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(
      config: Config
  ): Graph[N, E] =
    DefaultGraphImpl.fromWithoutCheck[N, E](nodes, edges)(config)

  override def from[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E[N]])(config: Config): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(config)
}

abstract class DefaultGraphImpl[N, E <: Edge[N]](
    iniNodes: Iterable[N] = Set[N](),
    iniEdges: Iterable[E] = Set[E]()
)(_config: DefaultGraphImpl.Config with GenConstrainedConfig with AdjacencyListArrayConfig)
    extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
    with GraphTraversalImpl[N, E] {

  final override val graphCompanion = DefaultGraphImpl

  protected type Config = DefaultGraphImpl.Config
  final override def config = _config.asInstanceOf[graphCompanion.Config with Config]

  @inline final protected def newNodeSet: NodeSetT = new NodeSet
  @transient protected[this] var _nodes: NodeSetT  = newNodeSet
  @inline final override def nodes                 = _nodes

  @transient protected[this] var _edges: EdgeSetT = new EdgeSet
  @inline final override def edges                = _edges

  initialize(iniNodes, iniEdges)

  @inline final override def empty = DefaultGraphImpl.empty(config)

  @inline final override def clone: this.type =
    graphCompanion.fromWithoutCheck[N, E](nodes.outer, edges.outer)(config).asInstanceOf[this.type]

  @SerialVersionUID(8082L)
  protected class NodeBase(override val outer: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(outer, hints)
      with InnerNodeTraversalImpl

  type NodeT = NodeBase

  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}

object DefaultGraphImpl extends MutableGraphCompanion[DefaultGraphImpl] {
  override def empty[N, E <: Edge[N]](config: Config): DefaultGraphImpl[N, E] =
    fromWithoutCheck(Set.empty, Set.empty)(config)

  override protected[collection] def fromWithoutCheck[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(
      config: Config
  ): DefaultGraphImpl[N, E] =
    new UserConstrainedGraphImpl[N, E](nodes, edges)(config)

  final override def from[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(
      config: Config
  ): DefaultGraphImpl[N, E] = from_?(nodes, edges) getOrElse empty[N, E](config)

  def from_?[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(
      config: Config
  ): Either[ConstraintViolation, DefaultGraphImpl[N, E]] = {
    def emptyGraph = empty[N, E](config)

    if (nodes.isEmpty && edges.isEmpty) Right(empty[N, E](config))
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

@SerialVersionUID(7701L)
class UserConstrainedGraphImpl[N, E <: Edge[N]](iniNodes: Iterable[N] = Nil, iniEdges: Iterable[E[N]] = Nil)(
    _config: DefaultGraphImpl.Config
) extends DefaultGraphImpl[N, E](iniNodes, iniEdges)(_config)
    with UserConstrainedGraph[N, E, DefaultGraphImpl[N, E]] {

  final override val self              = this
  final override val constraintFactory = config.constraintCompanion
  final override val constraint        = constraintFactory(this)

  private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)

  private def readObject(in: ObjectInputStream): Unit = {
    this.++?(Nil)
    _nodes = newNodeSet
    _edges = new EdgeSet
    initializeFrom(in, _nodes, _edges)
  }
}
