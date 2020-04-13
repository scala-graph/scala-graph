package scalax.collection

import scalax.collection.Compat.IterableOnce
import scalax.collection.GraphEdge._

/*
  $define mapNodes       Computes a new graph by applying `fNode` to all nodes of this graph.
  $define mapEdges       Computes a new graph by applying `fNode` to all nodes and `fEdge` to all edges of this graph.
  $define fEdge         `fEdge` is expected to set the passed nodes.
                         In case it produces duplicates, the resulting graph's size will decrease.
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
    * yield any multy-edge as stipulated above. */
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

  def stringPrefix: String = "Graph"

  sealed trait InnerElem
  trait InnerNode extends InnerElem {

    /** The outer node as supplied by instantiation or addition. */
    def outer: N
  }

  trait InnerEdge extends InnerElem {

    /** The outer edge as supplied by instantiation or addition. */
    def outer: E
  }

  type NodeT <: InnerNode
  type EdgeT <: InnerEdge

  /** Iterator over all inner nodes and edges. */
  def iterator: Iterator[InnerElem]

  /** Iterable over all nodes and edges. */
  def toIterable: Iterable[InnerElem]

  /** Iterator over all inner nodes and edges. */
  def outerIterator: Iterator[OuterElem]

  /** Iterable over all nodes and edges. */
  def toOuterIterable: Iterable[OuterElem]

  /** Creates a new graph by adding all `edges` and `nodes` ommitting duplicates.
    * The new graph is upcasted if any of the arguments is an upcast of `N` or `E`.
    * Use `union` to concatenate all nodes and edges of another graph.
    *
    * @param edges to be concatenated.
    * @param isolatedNodes to be concatenated. Nodes that are implicitly defined by any edge in `edges` will be ignored.
    */
  def concat[N2 >: N, E2 >: E <: EdgeLike[N2]](edges: IterableOnce[E2], isolatedNodes: IterableOnce[N2] = Nil)(
      implicit e: E2 <:< EdgeLike[N2]): This[N2, E2]

  /** Alias for `concat`. */
  @inline final def ++[N2 >: N, E2 >: E <: EdgeLike[N2]](
      edges: IterableOnce[E2],
      isolatedNodes: IterableOnce[N2] = Nil)(implicit e: E2 <:< EdgeLike[N2]): This[N2, E2] =
    concat(edges, isolatedNodes)(e)

  /** Computes the union between this graph and `that` graph. */
  @inline final def union[N2 >: N, E2 >: E <: EdgeLike[N2]](that: Graph[N2, E2]): This[N2, E2] =
    concat(that.edges.toOuter, that.nodes.toOuter)

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
    * If both `fNode` and `fEdge` have default values the original graph is retained. */
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

  protected def removedAll(edges: IterableOnce[E], isolatedNodes: IterableOnce[N] = Nil): This[N, E]

  /** Computes a new graph that is the difference of this graph and `that` graph. */
  final def diff(that: Graph[N, E]): This[N, E] = removedAll(that.edges.toOuter, that.nodes.toOuter)

  /** Alias for `diff`. */
  @inline final def &~(that: Graph[N, E]): This[N, E] = this diff that

  /** Computes the intersection between this graph and `that` graph. */
  final def intersect(that: Graph[N, E]): This[N, E] = this filter (n => that(n.outer), e => that(e.outer))

  /** Alias for `intersect`. */
  @inline final def &(that: Graph[N, E]): This[N, E] = this intersect that

  /** Alias for `union`. */
  @inline final def |(that: Graph[N, E]): This[N, E] = this union that

  /** $mapNodes
    *
    * The edge type `E` of this graph is required to be generic meaning that it accepts ends of `Any` type.
    * Edge types will be preserved and edge ends will reflect the mapped nodes.
    */
  def map[NN, EC[X] <: EdgeLike[X]](fNode: NodeT => NN)(implicit w1: E <:< GenericMapper,
                                                        w2: EC[N] =:= E,
                                                        fallbackMapper: EdgeCompanion[EC]): This[NN, EC[NN]]

  /** $mapNodes
    *
    * The target node type needs be of the type `N` or a subtype of `N` because, in case of typed edges,
    * a supertype potentially requires to replace not just edge ends but also edge types.
    * To map to a super type of `N` use `map` with a second parameter for the edge transformation.
    */
  def mapBounded[NN <: N, EC[X] <: EdgeLike[X]](fNode: NodeT => NN)(implicit w: E <:< PartialMapper,
                                                                    w2: EC[N] =:= E): This[NN, EC[NN]]

  /** $mapEdges
    * $fEdge
    */
  def map[NN, EC[X] <: AnyEdge[X]](fNode: NodeT => NN, edgeMapper: (NN, NN) => EC[NN]): This[NN, EC[NN]]

  /** $mapEdges
    * This method accepts a typed edge mapper.
    * $fEdge
    */
  def mapBounded[NN, EC <: AnyEdge[NN]](fNode: NodeT => NN, edgeMapper: (NN, NN) => EC): This[NN, EC]
}
