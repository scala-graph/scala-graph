package scalax.collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}
import scala.collection.Set
import scalax.collection.AnyGraph
import scalax.collection.generic.Edge
import scalax.collection.generic.ImmutableFactory
import scalax.collection.config.{AdjacencyListArrayConfig, GraphConfig}
import scalax.collection.mutable.{ArraySet, Builder}

trait Graph[N, E <: Edge[N]] extends AnyGraph[N, E] with GraphLike[N, E, Graph] with GraphOps[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E]
}

object Graph extends ImmutableFactory[Graph] {

  def empty[N, E <: Edge[N]](implicit config: GraphConfig = defaultConfig): Graph[N, E] =
    DefaultGraphImpl.empty[N, E](config)

  def from[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(implicit
      config: GraphConfig = defaultConfig
  ): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(config)

  override def from[N, E[X] <: Edge[X]](edges: Iterable[E[N]]): Graph[N, E[N]] =
    DefaultGraphImpl.from[N, E[N]](Nil, edges)(defaultConfig)
}

@SerialVersionUID(72L)
class DefaultGraphImpl[N, E <: Edge[N]](iniNodes: Iterable[N] = Set[N](), iniEdges: Iterable[E] = Set[E]())(implicit
    override val config: GraphConfig with AdjacencyListArrayConfig
) extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
    with GraphTraversalImpl[N, E] {
  final override val companion = DefaultGraphImpl

  @inline final protected def newNodeSet: NodeSetT       = new AdjacencyListNodeSet
  @transient private[this] var _nodes: NodeSetT          = newNodeSet
  @inline final override def nodes: AdjacencyListNodeSet = _nodes

  @transient private[this] var _edges: EdgeSetT          = new AdjacencyListEdgeSet
  @inline final override def edges: AdjacencyListEdgeSet = _edges

  initialize(iniNodes, iniEdges)

  override protected[this] def newBuilder          = new Builder[N, E, DefaultGraphImpl](DefaultGraphImpl)
  final override def empty: DefaultGraphImpl[N, E] = DefaultGraphImpl.empty[N, E]
  final override def copy(nodes: Iterable[N], edges: Iterable[E]) = DefaultGraphImpl.from[N, E](nodes, edges)

  @SerialVersionUID(7170L)
  final protected class NodeBase_(override val outer: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(outer, hints)
      with InnerNodeTraversalImpl {
    protected[collection] def asNodeT: NodeT = this
  }

  type NodeT = NodeBase_

  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)

  private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)

  private def readObject(in: ObjectInputStream): Unit = {
    _nodes = newNodeSet
    _edges = new AdjacencyListEdgeSet
    initializeFrom(in, _nodes, _edges)
  }
}

object DefaultGraphImpl extends ImmutableFactory[DefaultGraphImpl] {

  override def empty[N, E <: Edge[N]](implicit config: GraphConfig = defaultConfig) =
    new DefaultGraphImpl[N, E]()(coreConfig(config))

  override def from[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(implicit
      config: GraphConfig = defaultConfig
  ) =
    new DefaultGraphImpl[N, E](nodes, edges)(coreConfig(config))

  override def from[N, E[X] <: Edge[X]](edges: Iterable[E[N]]) =
    new DefaultGraphImpl[N, E[N]](Nil, edges)(defaultConfig)
}
