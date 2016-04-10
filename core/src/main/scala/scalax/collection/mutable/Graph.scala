package scalax.collection
package mutable

import scala.language.{higherKinds, postfixOps}
import scala.collection.generic.{CanBuildFrom, Growable, Shrinkable}
import scala.collection.mutable.{Builder, Cloneable, ArrayBuffer, Set => MutableSet}
import scala.reflect.ClassTag
import scala.math.max

import scalax.collection.{Graph => CommonGraph, GraphLike => CommonGraphLike}
import GraphPredef.{EdgeLikeIn, Param, InParam,
                    OuterNode, InnerNodeParam, OuterEdge, InnerEdgeParam} 
import immutable.{AdjacencyListBase}
import generic.{GraphCompanion, MutableGraphCompanion}
import config._
import GraphEdge.{EdgeCompanionBase, UnDiEdge}
import io._

protected[collection]
abstract class BuilderImpl[N,
                           E[X] <: EdgeLikeIn[X],
                           CC[N,E[X] <: EdgeLikeIn[X]] <: CommonGraph[N,E] with
                                                          CommonGraphLike[N,E,CC]](
    implicit edgeT: ClassTag[E[N]],
    config: GraphConfig)
    extends Builder[Param[N,E], CC[N,E]] {

  protected type This = CC[N,E]
  protected val nodes = new ArrayBuffer[N]   (config.orderHint)
  protected val edges = new ArrayBuffer[E[N]](
    config match {
      case CoreConfig(order, adjList) => order * max(adjList.initialCapacity, 16)
      case _ => config.orderHint * 32
    }
  )

  protected def add(elem: Param[N,E]) {
    elem match {
      case n: OuterNode [N]     => nodes += n.value
      case n: InnerNodeParam[N] => nodes += n.value
      case e: OuterEdge[N,E]          => edges += e.edge
      case e: InnerEdgeParam[N,E,_,E] => edges += e.asEdgeTProjection[N,E].toOuter
    }
  }

  override def +=(elem: Param[N,E]): this.type = {
    add(elem)
    this
  }

  /* overridden for increased performance */
  override def ++=(elems: TraversableOnce[Param[N,E]]): this.type = {
    elems foreach add
    this
  }

  def clear() {
    nodes.clear
    edges.clear
  }
}
class GraphBuilder[N,
                   E[X] <: EdgeLikeIn[X],
                   CC[N,E[X] <: EdgeLikeIn[X]]
                        <: CommonGraphLike[N,E,CC] with CommonGraph[N,E]]
      (companion: GraphCompanion[CC])
      (implicit edgeT: ClassTag[E[N]],
       config: GraphConfig)
  extends BuilderImpl[N,E,CC]
{
  def result: This =
    companion.from(nodes, edges)(edgeT, config.asInstanceOf[companion.Config])
}
/**
 * Trait with common mutable Graph methods.
 * 
 * @author Peter Empen
 */
trait GraphLike[N,
                E[X] <: EdgeLikeIn[X],
               +This[X, Y[X]<:EdgeLikeIn[X]] <: GraphLike[X,Y,This] with Graph[X,Y]]
	extends	CommonGraphLike[N, E, This]
	with	  Growable  [Param[N,E]]
	with	  Shrinkable[Param[N,E]] 
	with	  Cloneable [Graph[N,E]]
  with    EdgeOps   [N,E,This]
  with    Mutable
{ this: This[N,E] =>
	override def clone: This[N,E] =
    graphCompanion.from[N,E](nodes.toOuter, edges.toOuter)
	type NodeT <: InnerNode 
  trait InnerNode extends super.InnerNode with InnerNodeOps {
    this: NodeT =>
  }
  type NodeSetT <: NodeSet
	trait NodeSet extends MutableSet[NodeT] with super.NodeSet
	{
		@inline final override def -= (node: NodeT): this.type = { remove(node); this }
    @inline final def -?=         (node: NodeT): this.type = { removeGently(node); this }
    @inline final def -?          (node: NodeT)            = {
      val c = copy
      c subtract (node, false, minus, (NodeT) => {})
      c
    }
		override def remove(node: NodeT) = subtract(node, true,  minus, minusEdges)
    def removeGently   (node: NodeT) = subtract(node, false, minus, minusEdges)
    /**
     * removes all incident edges of `node` from the edge set leaving the node set unchanged.
     * @param node the node the incident edges of which are to be removed from the edge set.
     */
    protected def minusEdges(node: NodeT): Unit
	}
  type EdgeSetT <: EdgeSet
	trait EdgeSet extends MutableSet[EdgeT] with super.EdgeSet {
    @inline final def += (edge: EdgeT): this.type = { add(edge); this }
    /**
     * Same as `upsert` at graph level.
     */
    def upsert(edge: EdgeT): Boolean
    @inline final def -= (edge: EdgeT): this.type = { remove(edge); this }
    def removeWithNodes(edge: EdgeT): Boolean
  }

	override def ++=(xs: TraversableOnce[Param[N,E]]): this.type = { xs foreach += ; this } 
	
  /** Adds a node to this graph.
   *
   *  @param n the node to be added
   *  @return `true` if the node was not yet present in the graph, `false` otherwise.
   */
  def add(node: N): Boolean
  /**
   * Adds the given node if not yet present and returns it as an inner node.
   * 
   * @param node the node to add.
   * @return inner node containing the added node.
   */
  @inline final def addAndGet(node: N): NodeT = { add(node); find(node) get }
	def +  (node: N) = if (nodes contains Node(node)) this.asInstanceOf[This[N,E]]
	                   else clone += node
	@inline final def += (node: N): this.type = { add(node); this }
  def add(edge: E[N]): Boolean
  /**
   * Adds the given edge if not yet present and returns it as an inner edge.
   * 
   * @param edge the edge to add.
   * @return the inner edge containing the added edge.
   */
  @inline final def addAndGet(edge: E[N]): EdgeT = { add(edge); find(edge) get }
	@inline final protected def +#  (edge: E[N]) = clone +=# edge
	protected def +=#(edge: E[N]): this.type
	def += (elem: Param[N,E]) =
    elem match {
      case n: OuterNode [N] => this += n.value	
      case n: InnerNodeParam[N] => this += n.value
      case e: OuterEdge[N,E]      => this +=# e.edge
      case e: InnerEdgeParam[N,E,_,E] => this +=# e.asEdgeTProjection[N,E].toOuter
    }
  /**
   * If an inner edge equaling to `edge` is present in this graph, it is replaced
   * by `edge`, otherwise `edge` will be inserted. Such an update may be useful
   * whenever non-key parts of an immutable edge are to be modified.
   * 
   * @param edge The edge to add to this graph.
   * @return `true` if `edge` has been inserted.
   */
  def upsert(edge: E[N]): Boolean

	@inline final def     - (node: N) = clone -= node
  @inline final def remove(node: N) = nodes find node exists (nodes remove _)
	@inline final def    -= (node: N): this.type = { remove(node); this }
  @inline final def    -?=(node: N): this.type = { removeGently(node); this }
  @inline final def    -? (node: N) = clone -?= node
  final def   removeGently(node: N) = nodes find node exists (nodes removeGently _) 

	@inline final           def -     (edge: E[N]) = clone -=# edge
  @inline final           def remove(edge: E[N]) = edges remove Edge(edge)
	@inline final protected def -=#   (edge: E[N]): this.type = { remove(edge); this }
  @inline final protected def -!=#  (edge: E[N]): this.type = { removeWithNodes(edge); this }
  @inline final           def -!    (edge: E[N]) = clone -!=# edge
  @inline final protected def -#    (edge: E[N]) = clone  -=# edge
  @inline final protected def -!#   (edge: E[N]) = clone -!=# edge
  @inline final def removeWithNodes (edge: E[N]) = edges removeWithNodes Edge(edge)

  def -= (elem: Param[N,E]): this.type = elem match {
    case n: OuterNode [N] => this -= n.value 
    case n: InnerNodeParam[N] => this -= n.value
    case e: OuterEdge[N,E]      => this -=# e.edge
    case e: InnerEdgeParam[N,E,_,E] => this -=# e.asEdgeTProjection[N,E].toOuter
	}
  def -!=(elem: Param[N,E]): this.type = elem match {
    case n: OuterNode [N] => this  -= n.value 
    case n: InnerNodeParam[N] => this  -= n.value
    case e: OuterEdge[N,E]      => this -!=# e.edge
    case e: InnerEdgeParam[N,E,_,E] => this -!=# e.asEdgeTProjection[N,E].toOuter
  }
  
  /** Shrinks this graph to its intersection with `coll`.
   *
   * @param coll Collection of nodes and/or edges to intersect with;
   * @return this graph shrinked by the nodes and edges not contained in `coll`.
   */
  def &=(coll: Iterable[Param[N,E]]): this.type = {
    val toKeep = MSet.empty[Param[N,E]] ++= coll
    val toRemove = new EqHashSet[Param[N,E]](order)
    def check(p: Param[N,E]): Unit = { if(! toKeep.contains(p)) toRemove += p }
    
    this foreach check
    toRemove foreach -=
    this
  }
  /**
   * Removes all elements of `coll` from this graph. Edges will be ripple removed.
   *
   * @param coll Collection of nodes and/or edges to be removed; if the element type is N,
   *             it is removed from the node set otherwise from the edge set.
   * @return this graph shrinked by the nodes and edges contained in `coll`.
   */
  @inline final
  def --!=(coll: Iterable[Param[N,E]]): This[N,E] =
    (this /: coll)(_ -!= _) 
}
/**
 * The main trait for mutable graphs bundling the functionality of traits concerned with
 * specific aspects.
 * 
 * @tparam N the type of the nodes (vertices) in this graph.
 * @tparam E the kind of the edges in this graph. 
 *
 * @author Peter Empen
 */
trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	CommonGraph[N,E]
	with	  GraphLike[N, E, Graph]
{
	override def empty: Graph[N,E] = Graph.empty[N,E]
}
/**
 * The main companion object for mutable graphs.
 *
 * @author Peter Empen
 */
object Graph
  extends MutableGraphCompanion[Graph]
{
	def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                      config: Config = defaultConfig): Graph[N,E] =
	  new DefaultGraphImpl[N,E]()(edgeT, config)
  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N] = Nil,
                                              edges: Traversable[E[N]])
                                             (implicit edgeT: ClassTag[E[N]],
                                              config: Config = defaultConfig): Graph[N,E] =
    DefaultGraphImpl.from[N,E](nodes, edges)(
                               edgeT, config)
  implicit def cbfUnDi[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                                 config: Config = defaultConfig) =
    new GraphCanBuildFrom[N,E]()(edgeT, config).asInstanceOf[
      GraphCanBuildFrom[N,E]
      with CanBuildFrom[Graph[_,UnDiEdge], Param[N,E], Graph[N,E]]]
}
@SerialVersionUID(73L)
class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
    ( iniNodes: Traversable[N]    = Set[N](),
      iniEdges: Traversable[E[N]] = Set[E[N]]())
    ( implicit override val edgeT: ClassTag[E[N]],
      override val config: DefaultGraphImpl.Config with AdjacencyListArrayConfig)
  extends Graph[N,E]
     with AdjacencyListGraph[N, E, DefaultGraphImpl]
     with GraphTraversalImpl[N,E]
{
  override final val graphCompanion = DefaultGraphImpl
  protected type Config = DefaultGraphImpl.Config

  @inline final def newNodeSet: NodeSetT = new NodeSet
  override final val nodes = newNodeSet 
  override final val edges = new EdgeSet 
  initialize(iniNodes, iniEdges)

  override protected[this] def newBuilder = new GraphBuilder[N,E,DefaultGraphImpl](DefaultGraphImpl)
  @inline final override def empty: DefaultGraphImpl[N,E] = DefaultGraphImpl.empty[N,E]
  @inline final override def clone(): this.type = super.clone.asInstanceOf[this.type]
                                                                                                                                
  @SerialVersionUID(7370L)
  final protected class NodeBase(value: N, hints: ArraySet.Hints)
    extends InnerNodeImpl(value, hints)
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
}
object DefaultGraphImpl
  extends MutableGraphCompanion[DefaultGraphImpl]
{
	def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                      config: Config = defaultConfig) =
	  new DefaultGraphImpl[N,E]()(edgeT, config)
  override def from[N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N] = Nil,
                                              edges: Traversable[E[N]])
                                             (implicit edgeT: ClassTag[E[N]],
                                              config: Config = defaultConfig) =
    new DefaultGraphImpl[N,E](nodes, edges)(
                              edgeT, config)
}