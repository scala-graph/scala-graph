package scalax.collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.Set

import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.generic.ImmutableGraphCompanion
import scalax.collection.config.AdjacencyListArrayConfig
import scalax.collection.mutable.{ArraySet, Builder}

trait Graph[N, E <: EdgeLike[N]] extends AnyGraph[N, E] with GraphLike[N, E, Graph] with GraphOps[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E]
}

object Graph extends ImmutableGraphCompanion[Graph] {

  def empty[N, E <: EdgeLike[N]](implicit config: Config = defaultConfig): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](config)

  override def from[N, E <: EdgeLike[N]](nodes: Iterable[N], edges: Iterable[E])(implicit
      config: Config = defaultConfig
  ): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(config)

  override def from[N, E[X] <: EdgeLike[X]](edges: Iterable[E[N]]) =
    DefaultGraphImpl.from[N, E[N]](Nil, edges)(defaultConfig)
}

@SerialVersionUID(72L)
class DefaultGraphImpl[N, E <: EdgeLike[N]](iniNodes: Iterable[N] = Set[N](), iniEdges: Iterable[E] = Set[E]())(implicit
    override val config: DefaultGraphImpl.Config with AdjacencyListArrayConfig
) extends Graph[N, E]
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

  override protected[this] def newBuilder          = new Builder[N, E, DefaultGraphImpl](DefaultGraphImpl)
  final override def empty: DefaultGraphImpl[N, E] = DefaultGraphImpl.empty[N, E]
  final override def copy(nodes: Iterable[N], edges: Iterable[E]) = DefaultGraphImpl.from[N, E](nodes, edges)

  @SerialVersionUID(7170L)
  final protected class NodeBase(override val outer: N, hints: ArraySet.Hints) extends InnerNodeImpl(outer, hints)
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

  override def empty[N, E <: EdgeLike[N]](implicit config: Config = defaultConfig) =
    new DefaultGraphImpl[N, E](config)

  override def from[N, E <: EdgeLike[N]](nodes: Iterable[N], edges: Iterable[E])(implicit
      config: Config = defaultConfig
  ) =
    new DefaultGraphImpl[N, E](nodes, edges)(config)

  override def from[N, E[X] <: EdgeLike[X]](edges: Iterable[E[N]]) =
    new DefaultGraphImpl[N, E[N]](Nil, edges)(defaultConfig)
}
