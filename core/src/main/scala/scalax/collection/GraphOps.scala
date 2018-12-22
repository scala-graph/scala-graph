package scalax.collection

import scala.language.higherKinds

import scalax.collection.GraphEdge.EdgeLike

trait GraphOps[N, E[X] <: EdgeLike[X], +This[X, Y[X] <: EdgeLike[X]]] extends OuterElems[N, E] {

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
    /** The outer node as supplied at instantiation or addition to this graph. */
    def outer: N
  }

  trait InnerEdge extends InnerElem {
    /** The outer edge as supplied at instantiation or addition to this graph. */
    def outer: E[N]
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

  /** Whether the given outer node is contained in this graph. */
  def contains(node: N): Boolean

  /** Whether the given outer edge is contained in this graph. */
  def contains(edge: E[N]): Boolean

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
  @inline final def apply(node: N): Boolean = find(node).isDefined

  /** Whether the given edge is contained in this graph. */
  @inline final def apply(edge: E[N]): Boolean = find(edge).isDefined

  /** Computes a new graph with nodes satisfying `fNode` and edges staisfying `fEdge`. */
  def filter(fNode: NodePredicate = anyNode, fEdge: EdgePredicate = anyEdge): This[N, E] = ???

  /** Searches this graph for an inner node that wraps an outer node equalling to the given outer node. */
  def find(node: N): Option[NodeT]

  /** Searches this graph for an inner edge that wraps an outer edge equalling to the given outer edge. */
  def find(edge: E[N]): Option[EdgeT]

  /** Short for `find(node).get`.
    * @throws NoSuchElementException if the node is not found.
    */
  def get(node: N): NodeT

  /** Short for `find(edge).get`.
    * @throws NoSuchElementException if the edge is not found.
    */
  def get(edge: E[N]): EdgeT

  /** Computes a new graph that is the difference of this graph and `that` graph. */
  def diff(that: Graph[N, E]): This[N, E] = ???

  /** Alias for `diff`. */
  @inline final def &~(that: Graph[N, E]): This[N, E] = this diff that

  /** Computes the intersection between this graph and `that` graph. */
  def intersect(that: Graph[N, E]): This[N, E] = ???

  /** Alias for `intersect`. */
  @inline final def &(that: Graph[N, E]): This[N, E] = this intersect that

  /** Computes the union between this graph and `that` graph. */
  def union(that: Graph[N, E]): This[N, E] = ???

  /** Alias for `union`. */
  @inline final def |(that: Graph[N, E]): This[N, E] = this union that

  /** Computes a new graph by applying `fNode` to all nodes of this graph with edges preserved. */
  def mapNodes[NN](fNode: N => NN): This[NN, E] = ???

  /** Computes a new graph by applying `fEdge` to all edges of this graph with nodes preserved. */
  def mapEdges[NE[X] <: EdgeLike[X]](fEdge: E[N] => NE[N]): This[N, NE] = ???

  /** Computes a new graph by applying `fNode` to all nodes and `fEdge` to all edges of this graph. */
  def mapElements[NN, NE[X] <: EdgeLike[X]](fNode: N => NN, fEdge: E[N] => NE[NN]): This[NN, NE] = ???
}
