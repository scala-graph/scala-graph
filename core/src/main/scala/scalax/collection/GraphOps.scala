package scalax.collection

import scala.language.higherKinds

import scalax.collection.GraphEdge.EdgeLike

trait GraphOps[N, E[X] <: EdgeLike[X]] {

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
  trait InnerNode extends InnerElem
  trait InnerEdge extends InnerElem

  type NodeT <: InnerNode
  type EdgeT <: InnerEdge

  /** Iterator over all inner nodes and edges. */
  def iterator: Iterator[InnerElem]

  /** Iterable over all nodes and edges. */
  def toIterable: Iterable[InnerElem]

  /** Whether the given outer node is contained in this graph. */
  def contains(node: N): Boolean

  /** Whether the given outer edge is contained in this graph. */
  def contains(edge: E[N]): Boolean

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

  /** Short for `find(node).getOrElse(default)`. */
  final def getOrElse(node: N, default: NodeT): NodeT = find(node).getOrElse(default)

  /** Short for `find(node).getOrElse(default)`. */
  final def getOrElse(outerEdge: E[N], default: EdgeT): EdgeT = find(outerEdge).getOrElse(default)
}
