package scalax.collection

import scalax.collection.GraphEdge._

/* Operations common to mutable and immutable graphs.

  $define mapNodes  computes a new graph by applying `fNode` to all nodes of this graph
  $define mapEdges  computes a new graph by applying `fNode` to all nodes and `fEdge` to all edges of this graph
  $define mapNN     the node type of the resulting graph. It may be unchanged or different from this graphs node type
  $define mapEC     the higher kind of the edge type parameter of this graph
  $define mapFNode  function to map the inner nodes of this graph to any type that becomes the node type of the resulting graph
  $define $mapFEdge function to map edges based on the mapped nodes. It is expected to set the node ends to the passed nodes.
                    If this expectation is not met the resulting graph may become deteriorated.
 */
trait GraphOps[N, E <: EdgeLike[N], +This[X, Y <: EdgeLike[X]]] extends OuterElems[N, E] {

  /** Whether this graph contains any node or any edge. */
  @inline final def isEmpty: Boolean = iterator.isEmpty

  /** Whether all edges of this graph are directed. */
  def isDirected: Boolean

  /** Whether this graph contains at least one hyperedge. */
  def isHyper: Boolean

  /** Whether this graph contains at least one directed and one undirected edge. */
  def isMixed: Boolean

  /** Whether this graph contains at least one multi-edge. We defnie multi-edges by
    *    a. two or more directed edges having the same source and target
    *    a. two or more undirected edges connecting the same nodes
    *    a. two or more (directed) hyperedges that, after being decomposed into (directed) edges,
    * yield any multy-edge as stipulated above.
    */
  def isMulti: Boolean

  /** `true` if this graph has at most 1 node. */
  @inline final def isTrivial: Boolean = order <= 1

  /** `true` if this graph has at least 2 nodes. */
  @inline final def nonTrivial: Boolean = !isTrivial

  /** The order, commonly referred to as |G|, of this graph equaling to the number of nodes. */
  def order: Int

  /** The size, commonly referred to as ||G||, of this graph equaling to the number of edges. */
  def size: Int

  /** The number of nodes and edges. */
  @inline final def elementCount: Int = order + size

  /** The Sum of the weight of all edges. */
  def totalWeight: Double

  protected def className: String = "Graph"

  sealed trait InnerElem

  type NodeT <: InnerNode
  trait InnerNode extends InnerElem {

    /** The outer node as supplied by instantiation or addition. */
    def outer: N

    protected[collection] def asNodeT: NodeT
  }
  object InnerNode {
    def unapply(node: InnerNode): Option[N] = Some(node.outer)
  }

  type EdgeT <: InnerEdge
  trait InnerEdge extends InnerElem {

    /** The outer edge as supplied by instantiation or addition. */
    def outer: E

    protected[collection] def asEdgeT: EdgeT
  }
  object InnerEdge {
    def unapply(edge: InnerEdge): Option[E] = Some(edge.outer)
  }

  /** Iterator over all inner nodes and edges. */
  def iterator: Iterator[InnerElem]

  /** Iterable over all nodes and edges. */
  def toIterable: Iterable[InnerElem]

  /** Iterator over all inner nodes and edges. */
  def outerIterator: Iterator[OuterElem]

  /** Iterable over all nodes and edges. */
  def toOuterIterable: Iterable[OuterElem]

  /** Creates a new graph by adding all `edges` and `isolatedNodes` omitting duplicates.
    * The new graph is upcasted if any of the arguments is an upcast of `N` respectively `E`.
    * Use `union` to concatenate all nodes and edges of another graph.
    *
    * @param isolatedNodes to be concatenated. Nodes that are implicitly defined by any edge in `edges` will be ignored.
    * @param edges to be concatenated.
    */
  def concat[N2 >: N, E2 >: E <: EdgeLike[N2]](isolatedNodes: IterableOnce[N2], edges: IterableOnce[E2])(implicit
      e: E2 <:< EdgeLike[N2]
  ): This[N2, E2]

  /** Same as `concat(isolatedNodes, edges)` but with empty `isolatedNodes`.
    * This method is useful if you don't need to pass any isolated node.
    */
  def concat[N2 >: N, E2 >: E <: EdgeLike[N2]](edges: IterableOnce[E2])(implicit e: E2 <:< EdgeLike[N2]): This[N2, E2] =
    concat[N2, E2](Nil, edges)(e)

  /** Alias for `concat(isolatedNodes, edges)`. */
  @inline final def ++[N2 >: N, E2 >: E <: EdgeLike[N2]](isolatedNodes: IterableOnce[N2], edges: IterableOnce[E2])(
      implicit e: E2 <:< EdgeLike[N2]
  ): This[N2, E2] =
    concat(isolatedNodes, edges)(e)

  /** Alias for `concat(edges)`. */
  @inline final def ++[N2 >: N, E2 >: E <: EdgeLike[N2]](edges: IterableOnce[E2])(implicit
      e: E2 <:< EdgeLike[N2]
  ): This[N2, E2] =
    concat[N2, E2](edges)(e)

  /** Computes the union between this graph and `that` graph. */
  @inline final def union[N2 >: N, E2 >: E <: EdgeLike[N2]](that: Graph[N2, E2]): This[N2, E2] =
    concat(that.nodes.outerIterator, that.edges.outerIterator)

  /** Whether the given outer node is contained in this graph. */
  def contains(node: N): Boolean

  /** Whether the given outer edge is contained in this graph. */
  def contains(edge: E): Boolean

  type NodePredicate = NodeT => Boolean
  type EdgePredicate = EdgeT => Boolean

  /** Node predicate with constant `true`. */
  def anyNode: NodePredicate

  /** Node predicate with constant `false`. */
  def noNode: NodePredicate

  /** Edge predicate with constant `true`. */
  def anyEdge: EdgePredicate

  /** Edge predicate with constant `false`. */
  def noEdge: EdgePredicate

  /** Whether the given node is contained in this graph. */
  // TODO make implementation allocation-free
  @inline final def apply(node: N): Boolean = find(node).isDefined

  /** Whether the given edge is contained in this graph. */
  @inline final def apply(edge: E): Boolean = find(edge).isDefined

  /** Computes a new graph with nodes satisfying `fNode` and edges satisfying `fEdge`.
    * If both `fNode` and `fEdge` have default values the original graph is retained.
    */
  def filter(fNode: NodePredicate = anyNode, fEdge: EdgePredicate = anyEdge): This[N, E]

  /** Searches this graph for an inner node that wraps an outer node equalling to the given outer node. */
  def find(node: N): Option[NodeT]

  /** Searches this graph for an inner edge that wraps an outer edge equalling to the given outer edge. */
  def find(edge: E): Option[EdgeT]

  /** Short for `find(node).get`.
    *
    * @throws NoSuchElementException if the node is not found.
    */
  def get(node: N): NodeT

  /** Short for `find(edge).get`.
    *
    * @throws NoSuchElementException if the edge is not found.
    */
  def get(edge: E): EdgeT

  protected def removedAll(isolatedNodes: IterableOnce[N], edges: IterableOnce[E]): This[N, E]

  /** Computes a new graph that is the difference of this graph and `that` graph. */
  final def diff(that: Graph[N, E]): This[N, E] = removedAll(that.nodes.outerIterator, that.edges.outerIterator)

  /** Alias for `diff`. */
  @inline final def &~(that: Graph[N, E]): This[N, E] = this diff that

  /** Computes the intersection between this graph and `that` graph. */
  final def intersect(that: Graph[N, E]): This[N, E] = this filter (n => that(n.outer), e => that(e.outer))

  /** Alias for `intersect`. */
  @inline final def &(that: Graph[N, E]): This[N, E] = this intersect that

  /** Alias for `union`. */
  @inline final def |(that: Graph[N, E]): This[N, E] = this union that

  /** $mapNodes
    * The type parameter `E` of this graph is required to be generic meaning that `E` accepts ends of `Any` type.
    * Nonetheless, the type parameter `N` may be of any type.
    * Being `E` a generic edge type, you can map nodes to any type.
    *
    * @tparam NN            $mapNN
    * @tparam EC            $mapEC for use by `w2`
    * @param fNode          $mapFNode
    * @param w1             ensures that `E` of this graph is of type `GenericMapper`
    * @param w2             catches the current higher kind of `E` for use by `fallbackMapper`
    * @param fallbackMapper in case this graph contains generic and typed edges,
    *                       this mapper is used to replace typed edges by generic ones if necessary
    * @return               the mapped graph with a possibly changed node type parameter.
    *                       Edge ends reflect the mapped nodes while edge types will be preserved as far as possible.
    */
  def map[NN, EC[X] <: EdgeLike[X]](
      fNode: NodeT => NN
  )(implicit w1: E <:< GenericMapper, w2: EC[N] =:= E, fallbackMapper: EdgeCompanion[EC]): This[NN, EC[NN]]

  /** $mapNodes
    * Use this method to map a typed graph to a resulting typed graph bounded to the same edge type.
    * The target node type needs be of type `N` or a subtype of it
    * because a supertype requires to replace not just edge ends but also edge types.
    *
    * @see #map(NodeT => NN, (NN, NN) => EC[NN])
    * @see #mapBounded(NodeT => NN, (NN, NN) => EC)
    *
    * @tparam NN   $mapNN
    * @tparam EC   $mapEC for use by `w2`
    * @param fNode $mapFNode
    * @param w1    ensures that `E` of this graph is of type `PartialMapper`
    * @param w2    catches the current higher kind of `E` to determine the return type
    * @return      the mapped graph with a possibly downcasted node type parameter
    */
  def mapBounded[NN <: N, EC[X] <: EdgeLike[X]](
      fNode: NodeT => NN
  )(implicit w1: E <:< PartialMapper, w2: EC[N] =:= E): This[NN, EC[NN]]

  /** $mapEdges
    * Use this method to map nodes and edges to a graph with an edge type having one type parameter like `DiEdge[N]`.
    *
    * @see #mapBounded(NodeT => NN, (NN, NN) => EC)
    *
    * @tparam NN   $mapNN
    * @tparam EC   $mapEC for use by `w2`
    * @param fNode $mapFNode
    * @param fEdge $mapFEdge
    * @return      the mapped graph with a possibly changed node type and edge type parameter
    */
  def map[NN, EC[X] <: AnyEdge[X]](fNode: NodeT => NN, fEdge: (NN, NN) => EC[NN]): This[NN, EC[NN]]

  /** $mapEdges
    * Use this method to map nodes and edges to a graph with an edge type having no type parameter as typed edges usually do.
    *
    * @see #map(NodeT => NN, (NN, NN) => EC)
    *
    * @tparam NN   $mapNN
    * @tparam EC   $mapEC for use by `w2`
    * @param fNode $mapFNode
    * @param fEdge $mapFEdge
    * @return      the mapped graph with a possibly changed node type and edge type parameter
    */
  def mapBounded[NN, EC <: AnyEdge[NN]](fNode: NodeT => NN, fEdge: (NN, NN) => EC): This[NN, EC]
}
