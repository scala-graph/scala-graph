package scalax.collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.Set
import scala.reflect.ClassTag

import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.generic.ImmutableGraphCompanion
import scalax.collection.config.AdjacencyListArrayConfig
import scalax.collection.mutable.{ArraySet, Builder}

trait Graph[N, E <: EdgeLike[N]] extends AnyGraph[N, E] with GraphLike[N, E, Graph] with GraphOps[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E]
}

object Graph extends ImmutableGraphCompanion[Graph] {

  def empty[N, E <: EdgeLike[N]](implicit edgeT: ClassTag[E], config: Config = defaultConfig): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](edgeT, config)

  override def from[N, E <: EdgeLike[N]](nodes: Traversable[N], edges: Traversable[E])(
      implicit edgeT: ClassTag[E],
      config: Config = defaultConfig): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(edgeT, config)

  override def from[N, E[X] <: EdgeLike[X]](edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]]) =
    DefaultGraphImpl.from[N, E[N]](Nil, edges)(edgeT, defaultConfig)
}

@SerialVersionUID(72L)
class DefaultGraphImpl[N, E <: EdgeLike[N]](iniNodes: Traversable[N] = Set[N](), iniEdges: Traversable[E] = Set[E]())(
    implicit override val edgeT: ClassTag[E],
    override val config: DefaultGraphImpl.Config with AdjacencyListArrayConfig)
    extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
// TODO     with GraphTraversalImpl[N,E]
    {
  final override val companion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config

  @inline final protected def newNodeSet: NodeSetT = new NodeSet
  @transient private[this] var _nodes: NodeSetT    = newNodeSet
  @inline final override def nodes: NodeSet        = _nodes

  @transient private[this] var _edges: EdgeSetT = new EdgeSet
  @inline final override def edges: EdgeSet     = _edges

  initialize(iniNodes, iniEdges)

  override protected[this] def newBuilder                               = new Builder[N, E, DefaultGraphImpl](DefaultGraphImpl)
  final override def empty: DefaultGraphImpl[N, E]                      = DefaultGraphImpl.empty[N, E]
  final override def copy(nodes: Traversable[N], edges: Traversable[E]) = DefaultGraphImpl.from[N, E](nodes, edges)

  @SerialVersionUID(7170L)
  final protected class NodeBase(val outer: N, hints: ArraySet.Hints) extends InnerNodeImpl(outer, hints)
  /* TODO
      with InnerNodeTraversalImpl
   */

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

  override def empty[N, E <: EdgeLike[N]](implicit edgeT: ClassTag[E], config: Config = defaultConfig) =
    new DefaultGraphImpl[N, E]()(edgeT, config)

  override def from[N, E <: EdgeLike[N]](nodes: Traversable[N], edges: Traversable[E])(implicit edgeT: ClassTag[E],
                                                                                       config: Config = defaultConfig) =
    new DefaultGraphImpl[N, E](nodes, edges)(edgeT, config)

  override def from[N, E[X] <: EdgeLike[X]](edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]]) =
    new DefaultGraphImpl[N, E[N]](Nil, edges)(edgeT, defaultConfig)
}
