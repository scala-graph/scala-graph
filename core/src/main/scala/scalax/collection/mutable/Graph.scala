package scalax.collection
package mutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.mutable.ArrayBuffer
import scala.math.max
import scala.util.chaining._

import scalax.collection.{Graph => AnyGraph, GraphLike => AnyGraphLike}
import scalax.collection.generic.{AnyEdge, Edge}
import scalax.collection.generic.{GraphCompanion, MutableGraphCompanion}
import scalax.collection.config._

trait AbstractBuilder[N, E <: Edge[N]] extends Growable[N, E] {

  /** If an inner edge equaling to `edge` is present in this graph, it is replaced
    * by `edge`, otherwise `edge` will be inserted.
    * This is useful if non-key parts of an immutable edge are to be modified.
    * @return `true` if `edge` has been inserted, `false` if it has been replaced.
    */
  def upsert(edge: E): Boolean

  def clear(): Unit
}

abstract protected[collection] class BuilderImpl[N, E <: Edge[N]](implicit config: GraphConfig)
    extends AbstractBuilder[N, E] {

  protected val nodes = new ArrayBuffer[N](config.orderHint)
  protected val edges = new ArrayBuffer[E](
    config match {
      case CoreConfig(order, adjList) => order * max(adjList.initialCapacity, 16)
      case _                          => config.orderHint * 32
    }
  )

  protected def add(node: N): Boolean           = NeverUsed
  final override def addOne(node: N): this.type = { nodes += node; this }

  def add(edge: E): Boolean                     = NeverUsed
  final override def addOne(edge: E): this.type = { edges += edge; this }

  def upsert(edge: E): Boolean = edges contains edge pipe { found =>
    if (found) edges -= edge
    edges += edge
    !found
  }

  def clear(): Unit = {
    nodes.clear()
    edges.clear()
  }
}

class Builder[N, E <: Edge[N], +CC[N, E <: Edge[N]] <: AnyGraphLike[N, E, CC] with AnyGraph[N, E]](
    companion: GraphCompanion[CC]
)(implicit config: GraphConfig)
    extends BuilderImpl[N, E] {

  def result: CC[N, E] = companion.from(nodes, edges)(config.asInstanceOf[companion.Config])
}

/** Trait with common mutable Graph methods.
  *
  * @author Peter Empen
  */
trait GraphLike[N, E <: Edge[N], +CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with Graph[X, Y]]
    extends AnyGraphLike[N, E, CC]
    with GraphOps[N, E, CC]
    /* TODO
    with EdgeOps[N, E, CC]
     */ {
  selfGraph: CC[N, E] =>

  override def clone: CC[N, E] = companion.from[N, E](nodes.toOuter, edges.toOuter)

  type NodeT <: GraphLikeInnerNode
  trait GraphLikeInnerNode extends super.GraphInnerNode { // TODO with InnerNodeOps {
    this: NodeT =>

    /** Adds new non-hyper edges connecting `this` inner node with `those` inner nodes.
      */
    final def connectWith[AE <: E with AnyEdge[N]](n_1: NodeT, ns: NodeT*)(implicit
        edgeFactory: (N, N) => AE
    ): this.type = {
      def add(n: NodeT) = selfGraph.edges addOne newEdge(edgeFactory(this.outer, n.outer), this, n)
      add(n_1)
      ns foreach add
      this
    }

    /** Adds new non-hyper edges connecting `this` inner node with `those` outer nodes.
      */
    final def connectWith[AE <: E with AnyEdge[N]](n_1: N, ns: N*)(implicit factory: (N, N) => AE): this.type = {
      def inner(n: N) = selfGraph addAndGet n
      connectWith(inner(n_1), ns map inner: _*)
    }
  }

  type NodeSetT <: GraphLikeNodeSet
  trait GraphLikeNodeSet extends MSet[NodeT] with GraphNodeSet {
    override def remove(node: NodeT): Boolean = subtract(node, rippleDelete = true, minus, minusEdges)

    /** removes all incident edges of `node` from the edge set leaving the node set unchanged.
      *
      * @param node the node the incident edges of which are to be removed from the edge set.
      */
    protected def minusEdges(node: NodeT): Unit

    override def clear(): Unit                          = this foreach -=
    override def diff(that: AnySet[NodeT]): MSet[NodeT] = clone().filterInPlace(n => !that(n))
  }

  type EdgeSetT <: GraphLikeEdgeSet
  trait GraphLikeEdgeSet extends MSet[EdgeT] with GraphEdgeSet {
    @inline final def addOne(edge: EdgeT)      = { add(edge); this }
    @inline final def subtractOne(edge: EdgeT) = { remove(edge); this }

    /** Same as `upsert` at graph level. */
    def upsert(edge: EdgeT): Boolean
    def removeWithNodes(edge: EdgeT): Boolean

    override def clear(): Unit                          = this foreach -=
    override def diff(that: AnySet[EdgeT]): MSet[EdgeT] = clone().filterInPlace(n => !that(n))
  }
  def edges: EdgeSetT

  /** Adds the given node if not yet present and returns it as an inner node.
    *
    * @param node the node to add.
    * @return inner node containing the added node.
    */
  @inline final def addAndGet(node: N): NodeT = { add(node); find(node).get }

  /** Adds the given edge if not yet present and returns it as an inner edge.
    *
    * @param edge the edge to add.
    * @return the inner edge containing the added edge.
    */
  @inline final def addAndGet(edge: E): EdgeT = { add(edge); find(edge).get }

  @inline final def remove(node: N): Boolean        = nodes find node exists (nodes remove _)
  @inline final def subtractOne(node: N): this.type = { remove(node); this }

  @inline final def remove(edge: E): Boolean        = edges remove BaseInnerEdge(edge)
  @inline final def subtractOne(edge: E): this.type = { remove(edge); this }

  def filterInPlace(fNode: NodePredicate = anyNode, fEdge: EdgePredicate = anyEdge): this.type = {
    if (fNode ne anyNode) nodes filterInPlace fNode
    if (fEdge ne anyEdge) edges filterInPlace fEdge
    this
  }
}

/** The main trait for mutable graphs bundling the functionality of traits concerned with
  * specific aspects.
  *
  * @tparam N the type of the nodes (vertices) in this graph.
  * @tparam E the kind of the edges in this graph.
  * @author Peter Empen
  */
trait Graph[N, E <: Edge[N]] extends AnyGraph[N, E] with GraphLike[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E]
}

/** The main companion object for mutable graphs.
  *
  * @author Peter Empen
  */
object Graph extends MutableGraphCompanion[Graph] {

  def empty[N, E <: Edge[N]](implicit config: Config = defaultConfig): Graph[N, E] =
    new DefaultGraphImpl[N, E]()(config)

  override def from[N, E <: Edge[N]](nodes: Iterable[N] = Nil, edges: Iterable[E])(implicit
      config: Config = defaultConfig
  ): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(config)

  override def from[N, E[X] <: Edge[X]](edges: Iterable[E[N]]) =
    DefaultGraphImpl.from[N, E[N]](Nil, edges)(defaultConfig)
}

@SerialVersionUID(74L)
class DefaultGraphImpl[N, E <: Edge[N]](iniNodes: Iterable[N] = Set[N](), iniEdges: Iterable[E] = Set[E]())(implicit
    override val config: DefaultGraphImpl.Config with AdjacencyListArrayConfig
) extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
    with GraphTraversalImpl[N, E] {
  final override val companion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config

  @inline final protected def newNodeSet: NodeSetT = new AdjacencyListNodeSet

  @transient private[this] var _nodes: NodeSetT = newNodeSet

  @inline final override def nodes: AdjacencyListNodeSet = _nodes

  @transient private[this] var _edges: EdgeSetT = new AdjacencyListEdgeSet

  @inline final override def edges: AdjacencyListEdgeSet = _edges

  initialize(iniNodes, iniEdges)

  override protected[this] def newBuilder = new Builder[N, E, DefaultGraphImpl](DefaultGraphImpl)

  final override def empty: DefaultGraphImpl[N, E] = DefaultGraphImpl.empty[N, E]

  final override def clone: this.type = super.clone.asInstanceOf[this.type]

  @SerialVersionUID(7370L)
  final protected class NodeBase_(override val outer: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(outer, hints)
      with InnerNodeTraversalImpl {
    final protected[collection] def asNodeT: NodeT = this
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

object DefaultGraphImpl extends MutableGraphCompanion[DefaultGraphImpl] {

  def empty[N, E <: Edge[N]](implicit config: Config = defaultConfig) =
    new DefaultGraphImpl[N, E]()(config)

  override def from[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(implicit
      config: Config = defaultConfig
  ) =
    new DefaultGraphImpl[N, E](nodes, edges)(config)

  override def from[N, E[X] <: Edge[X]](edges: Iterable[E[N]]) =
    new DefaultGraphImpl[N, E[N]](Nil, edges)(defaultConfig)
}
