package scalax.collection.constrained
package mutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.language.postfixOps
import scala.collection.Set
import scala.reflect.ClassTag

import scalax.collection.GraphTraversalImpl
import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.mutable.{ArraySet, BuilderImpl}
import scalax.collection.config.AdjacencyListArrayConfig
import scalax.collection.constrained.{Graph => CGraph, GraphLike => CGraphLike}
import PreCheckFollowUp._
import generic.GraphConstrainedCompanion
import config.GenConstrainedConfig

class GraphBuilder[N, E[+X] <: EdgeLikeIn[X], GC[N, E[+X] <: EdgeLikeIn[X]] <: CGraph[N, E] with CGraphLike[N, E, GC]](
    companion: GraphConstrainedCompanion[GC])(implicit edgeT: ClassTag[E[N]], config: GenConstrainedConfig)
    extends BuilderImpl[N, E, GC] {
  def result: This =
    companion.from(nodes, edges)(edgeT, config.asInstanceOf[companion.Config])
}

import scalax.collection.constrained.generic.MutableGraphCompanion

trait Graph[N, E[+X] <: EdgeLikeIn[X]]
    extends scalax.collection.mutable.Graph[N, E]
    with scalax.collection.constrained.Graph[N, E]
    with GraphLike[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E](edgeT, config)
}

object Graph extends MutableGraphCompanion[Graph] {
  override def empty[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](edgeT, config)

  override protected[collection] def fromWithoutCheck[N, E[+X] <: EdgeLikeIn[X]](
      nodes: Iterable[N],
      edges: Iterable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): Graph[N, E] =
    DefaultGraphImpl.fromWithoutCheck[N, E](nodes, edges)(edgeT, config)

  override def from[N, E[+X] <: EdgeLikeIn[X]](nodes: Iterable[N], edges: Iterable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(edgeT, config)

  // TODO: canBuildFrom
}

abstract class DefaultGraphImpl[N, E[+X] <: EdgeLikeIn[X]](iniNodes: Iterable[N] = Set[N](),
                                                           iniEdges: Iterable[E[N]] = Set[E[N]]())(
    implicit override val edgeT: ClassTag[E[N]],
    _config: DefaultGraphImpl.Config with GenConstrainedConfig with AdjacencyListArrayConfig)
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

  @inline final override def empty = DefaultGraphImpl.empty(edgeT, config)

  @inline final override def clone: this.type =
    graphCompanion.fromWithoutCheck[N, E](nodes.toOuter, edges.toOuter)(edgeT, config).asInstanceOf[this.type]

  @SerialVersionUID(8082L)
  protected class NodeBase(value: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(value, hints)
      with InnerNodeTraversalImpl

  type NodeT = NodeBase

  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}

object DefaultGraphImpl extends MutableGraphCompanion[DefaultGraphImpl] {
  override def empty[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                                config: Config): DefaultGraphImpl[N, E] =
    fromWithoutCheck(Set.empty, Set.empty)(edgeT, config)

  override protected[collection] def fromWithoutCheck[N, E[+X] <: EdgeLikeIn[X]](
      nodes: Iterable[N],
      edges: Iterable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): DefaultGraphImpl[N, E] =
    new UserConstrainedGraphImpl[N, E](nodes, edges)(edgeT, config)

  final override def from[N, E[+X] <: EdgeLikeIn[X]](nodes: Iterable[N], edges: Iterable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): DefaultGraphImpl[N, E] = from_?(nodes, edges) getOrElse empty[N, E](edgeT, config)

  def from_?[N, E[+X] <: EdgeLikeIn[X]](nodes: Iterable[N], edges: Iterable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config): Either[ConstraintViolation, DefaultGraphImpl[N, E]] = {
    def emptyGraph = empty[N, E](edgeT, config)

    if (nodes.isEmpty && edges.isEmpty) Right(empty[N, E](edgeT, config))
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

@SerialVersionUID(7701L)
class UserConstrainedGraphImpl[N, E[+X] <: EdgeLikeIn[X]](iniNodes: Iterable[N] = Nil, iniEdges: Iterable[E[N]] = Nil)(
    implicit override val edgeT: ClassTag[E[N]],
    _config: DefaultGraphImpl.Config)
    extends DefaultGraphImpl[N, E](iniNodes, iniEdges)(edgeT, _config)
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
