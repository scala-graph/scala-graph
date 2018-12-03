package scalax.collection
package mutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.mutable.{ArrayBuffer, Cloneable, Set => MutableSet}
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.math.max

import scalax.collection.{Graph => CommonGraph, GraphLike => CommonGraphLike}
import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.generic.{GraphCompanion, MutableGraphCompanion}
import scalax.collection.config._

abstract protected[collection] class BuilderImpl[
    N,
    E[X] <: EdgeLike[X],
    +CC[N, E[X] <: EdgeLike[X]] <: CommonGraph[N, E] with CommonGraphLike[N, E, CC]](implicit edgeT: ClassTag[E[N]],
                                                                                     config: GraphConfig)
    extends Growable[N, E] {

  protected val nodes = new ArrayBuffer[N](config.orderHint)
  protected val edges = new ArrayBuffer[E[N]](
    config match {
      case CoreConfig(order, adjList) => order * max(adjList.initialCapacity, 16)
      case _                          => config.orderHint * 32
    }
  )

  def +=(node: N): this.type    = { nodes += node; this }
  def +=(edge: E[N]): this.type = { edges += edge; this }

  def clear() {
    nodes.clear
    edges.clear
  }
}

class Builder[N, E[X] <: EdgeLike[X], +CC[N, E[X] <: EdgeLike[X]] <: CommonGraphLike[N, E, CC] with CommonGraph[N, E]](
    companion: GraphCompanion[CC])(implicit edgeT: ClassTag[E[N]], config: GraphConfig)
    extends BuilderImpl[N, E, CC] {

  def result: CC[N, E] = companion.from(nodes, edges)(edgeT, config.asInstanceOf[companion.Config])
}

/** Trait with common mutable Graph methods.
  *
  * @author Peter Empen
  */
trait GraphLike[N, E[X] <: EdgeLike[X], +This[X, Y[X] <: EdgeLike[X]] <: GraphLike[X, Y, This] with Graph[X, Y]]
    extends CommonGraphLike[N, E, This]
    with Growable[N, E]
    with Shrinkable[N, E]
    with Cloneable[Graph[N, E]]
    /* TODO
    with EdgeOps[N, E, This]
     */
    with Mutable {
  this: // This[N,E] => see https://youtrack.jetbrains.com/issue/SCL-13199
  This[N, E] with GraphLike[N, E, This] with Graph[N, E] =>

  override def clone: This[N, E] = graphCompanion.from[N, E](nodes.toOuter, edges.toOuter)

  type NodeT <: InnerNode
  trait InnerNode extends super.InnerNode { // TODO with InnerNodeOps {
    this: NodeT =>
  }

  type NodeSetT <: NodeSet
  trait NodeSet extends MutableSet[NodeT] with super.NodeSet {
    @inline final override def -=(node: NodeT): this.type = { remove(node); this }
    override def remove(node: NodeT)                      = subtract(node, true, minus, minusEdges)

    /** removes all incident edges of `node` from the edge set leaving the node set unchanged.
      *
      * @param node the node the incident edges of which are to be removed from the edge set.
      */
    protected def minusEdges(node: NodeT): Unit
  }

  type EdgeSetT <: EdgeSet
  trait EdgeSet extends MutableSet[EdgeT] with super.EdgeSet {
    @inline final def +=(edge: EdgeT): this.type = { add(edge); this }

    /** Same as `upsert` at graph level.
      */
    def upsert(edge: EdgeT): Boolean
    @inline final def -=(edge: EdgeT): this.type = { remove(edge); this }
    def removeWithNodes(edge: EdgeT): Boolean
  }

  /** Adds a node to this graph.
    *
    *  @param node the node to be added
    *  @return `true` if the node was not yet present in the graph, `false` otherwise.
    */
  def add(node: N): Boolean

  /** Adds the given node if not yet present and returns it as an inner node.
    *
    * @param node the node to add.
    * @return inner node containing the added node.
    */
  @inline final def addAndGet(node: N): NodeT = { add(node); find(node).get }

  def +(node: N) =
    if (nodes contains Node(node)) this.asInstanceOf[This[N, E]]
    else clone += node

  def +(edge: E[N]) =
    if (edges contains Edge(edge)) this.asInstanceOf[This[N, E]]
    else clone += edge

  @inline final def +=(node: N): this.type    = { add(node); this }
  @inline final def +=(edge: E[N]): this.type = { this +=# edge; this }

  def add(edge: E[N]): Boolean

  /** Adds the given edge if not yet present and returns it as an inner edge.
    *
    * @param edge the edge to add.
    * @return the inner edge containing the added edge.
    */
  @inline final def addAndGet(edge: E[N]): EdgeT = { add(edge); find(edge).get }
  @inline final protected def +#(edge: E[N])     = clone +=# edge
  protected def +=#(edge: E[N]): this.type

  /** If an inner edge equaling to `edge` is present in this graph, it is replaced
    * by `edge`, otherwise `edge` will be inserted. Such an update may be useful
    * whenever non-key parts of an immutable edge are to be modified.
    *
    * @param edge The edge to add to this graph.
    * @return `true` if `edge` has been inserted.
    */
  def upsert(edge: E[N]): Boolean

  @inline final def -(node: N): This[N, E]   = clone -= node
  @inline final def remove(node: N): Boolean = nodes find node exists (nodes remove _)
  @inline final def -=(node: N): this.type   = { remove(node); this }

  @inline final def -(edge: E[N])             = clone -= edge
  @inline final def remove(edge: E[N])        = edges remove Edge(edge)
  @inline final def -=(edge: E[N]): this.type = { remove(edge); this }

  /** Shrinks this graph to its intersection with `coll`.
    *
    * @param coll Collection of nodes and/or edges to intersect with;
    * @return this graph shrinked by the nodes and edges not contained in `coll`.
    *TODO
    *def &=(coll: Iterable[OuterElem[N, E]]): this.type = {
    *val toKeep                      = MSet.empty[OuterElem[N, E]] ++= coll
    *val toRemove                    = new EqHashSet[OuterElem[N, E]](order)
    *def check(p: OuterElem[N, E]): Unit = if (!toKeep.contains(p)) toRemove += p
 **
    *this foreach check
    *toRemove foreach -=
    *this
    *}
    **/

  /** Removes all elements of `coll` from this graph. Edges will be ripple removed.
  *
  * @param coll Collection of nodes and/or edges to be removed; if the element type is N,
  *             it is removed from the node set otherwise from the edge set.
  * @return this graph shrinked by the nodes and edges contained in `coll`.
  *TODO
  *@inline final def --!=(coll: Iterable[Param[N, E]]): This[N, E] = (this /: coll)(_ -!= _)
    **/
}

/** The main trait for mutable graphs bundling the functionality of traits concerned with
  * specific aspects.
  *
  * @tparam N the type of the nodes (vertices) in this graph.
  * @tparam E the kind of the edges in this graph.
  * @author Peter Empen
  */
trait Graph[N, E[X] <: EdgeLike[X]] extends CommonGraph[N, E] with GraphLike[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E]
}

/** The main companion object for mutable graphs.
  *
  * @author Peter Empen
  */
object Graph extends MutableGraphCompanion[Graph] {

  def empty[N, E[X] <: EdgeLike[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig): Graph[N, E] =
    new DefaultGraphImpl[N, E]()(edgeT, config)

  override def from[N, E[X] <: EdgeLike[X]](nodes: Traversable[N] = Nil, edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config = defaultConfig): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(edgeT, config)
}

@SerialVersionUID(74L)
class DefaultGraphImpl[N, E[X] <: EdgeLike[X]](iniNodes: Traversable[N] = Set[N](),
                                               iniEdges: Traversable[E[N]] = Set[E[N]]())(
    implicit override val edgeT: ClassTag[E[N]],
    override val config: DefaultGraphImpl.Config with AdjacencyListArrayConfig)
    extends Graph[N, E]
    with AdjacencyListGraph[N, E, DefaultGraphImpl]
    /* TODO
    with GraphTraversalImpl[N, E]
     */ {
  final override val graphCompanion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config

  @inline final protected def newNodeSet: NodeSetT = new NodeSet
  @transient private[this] var _nodes: NodeSetT    = newNodeSet
  @inline final override def nodes                 = _nodes

  @transient private[this] var _edges: EdgeSetT = new EdgeSet
  @inline final override def edges              = _edges

  initialize(iniNodes, iniEdges)

  override protected[this] def newBuilder          = new Builder[N, E, DefaultGraphImpl](DefaultGraphImpl)
  final override def empty: DefaultGraphImpl[N, E] = DefaultGraphImpl.empty[N, E]
  final override def clone: this.type              = super.clone.asInstanceOf[this.type]

  @SerialVersionUID(7370L)
  final protected class NodeBase(val outer: N, hints: ArraySet.Hints) extends InnerNodeImpl(outer, hints)
// TODO      with    InnerNodeTraversalImpl

  type NodeT = NodeBase

  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)

  private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)

  private def readObject(in: ObjectInputStream): Unit = {
    _nodes = newNodeSet
    _edges = new EdgeSet
    initializeFrom(in, _nodes, _edges)
  }
}

object DefaultGraphImpl extends MutableGraphCompanion[DefaultGraphImpl] {

  def empty[N, E[X] <: EdgeLike[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig) =
    new DefaultGraphImpl[N, E]()(edgeT, config)

  override def from[N, E[X] <: EdgeLike[X]](nodes: Traversable[N] = Nil, edges: Traversable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config = defaultConfig) =
    new DefaultGraphImpl[N, E](nodes, edges)(edgeT, config)
}
