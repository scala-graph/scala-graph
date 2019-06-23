package scalax.collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.language.higherKinds
import scala.collection.Set
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

import scalax.collection.{Graph => CommonGraph}
import GraphEdge.UnDiEdge
import GraphPredef.{EdgeLikeIn, Param}
import generic.ImmutableGraphCompanion
import config.AdjacencyListArrayConfig
import mutable.{ArraySet, GraphBuilder}

/** The main trait for immutable graphs bundling functionality that is not specific to graph representation.
  */
trait Graph[N, E[X] <: EdgeLikeIn[X]] extends CommonGraph[N, E] with GraphLike[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E]
}
object Graph extends ImmutableGraphCompanion[Graph] {

  def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](edgeT, config)

  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N] = Nil, edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config = defaultConfig): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(edgeT, config)

  implicit def cbfUnDi[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig) =
    new GraphCanBuildFrom[N, E]()(edgeT, config)
      .asInstanceOf[GraphCanBuildFrom[N, E] with CanBuildFrom[Graph[_, UnDiEdge], Param[N, E], Graph[N, E]]]
}

@SerialVersionUID(72L)
class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]](iniNodes: Traversable[N] = Set[N](),
                                                 iniEdges: Traversable[E[N]] = Set[E[N]]())(
    implicit override val edgeT: ClassTag[E[N]],
    override val config: DefaultGraphImpl.Config with AdjacencyListArrayConfig)
    extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
    with GraphTraversalImpl[N, E] {

  final override val graphCompanion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config

  @inline final protected def newNodeSet: NodeSetT = new NodeSet
  @transient private[this] var _nodes: NodeSetT    = newNodeSet
  @inline final override def nodes                 = _nodes

  @transient private[this] var _edges: EdgeSetT = new EdgeSet
  @inline final override def edges              = _edges

  initialize(iniNodes, iniEdges)

  override protected[this] def newBuilder                                   = new GraphBuilder[N, E, DefaultGraphImpl](DefaultGraphImpl)
  final override def empty: DefaultGraphImpl[N, E]                          = DefaultGraphImpl.empty[N, E]
  final protected def copy(nodes: Traversable[N], edges: Traversable[E[N]]) = DefaultGraphImpl.from[N, E](nodes, edges)

  final protected def +#(e: E[N]): DefaultGraphImpl[N, E] =
    if (edges contains Edge(e)) this
    else copy(nodes.toOuter, edges.toOuter.toBuffer += e)

  protected def -#(e: E[N]): DefaultGraphImpl[N, E] =
    if (edges contains Edge(e)) copy(nodes.toOuter, edges.toOuter.toBuffer -= e)
    else this

  @SerialVersionUID(7170L)
  final protected class NodeBase(value: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(value, hints)
      with InnerNodeTraversalImpl

  type NodeT = NodeBase

  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)

  private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)

  private def readObject(in: ObjectInputStream): Unit = {
    _nodes = newNodeSet
    _edges = new EdgeSet
    initializeFrom(in, _nodes, _edges)
  }
}

object DefaultGraphImpl extends ImmutableGraphCompanion[DefaultGraphImpl] {

  override def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig) =
    new DefaultGraphImpl[N, E]()(edgeT, config)

  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N] = Nil, edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config = defaultConfig) =
    new DefaultGraphImpl[N, E](nodes, edges)(edgeT, config)

  implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                                      config: Config = defaultConfig): GraphCanBuildFrom[N, E] =
    new GraphCanBuildFrom[N, E]()(edgeT, config)
}
