package scalax.collection.immutable

import scala.collection.compat._
import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphLike

/** Immutable graph only operations.

    $define edgesOnlyUseCase Provided for the use case when you don't need to pass any isolated node.
  */
trait GraphOps[N, E <: EdgeLike[N], +This[X, Y <: EdgeLike[X]] <: GraphLike[X, Y, This] with Graph[X, Y]] {

  /** Creates a new supergraph with an additional node unless this graph already contains `node`. */
  def incl(node: N): This[N, E]

  /** Alias for `incl(node)`. */
  @inline final def +(node: N): This[N, E] = incl(node)

  /** Creates a new supergraph with an additional edge unless this graph already contains `edge`. */
  def incl(edge: E): This[N, E]

  /** Alias for `incl(edge)`. */
  @inline final def +(edge: E): This[N, E] = incl(edge)

  /** Creates a new graph with the elements of this graph minus `node` and its incident edges. */
  def excl(node: N): This[N, E]

  /** Alias for `excl(node)`. */
  @inline final def -(node: N): This[N, E] = excl(node)

  /** Creates a new graph with the elements of this graph minus `edge`. */
  def excl(edge: E): This[N, E]

  /** Alias for `incl(edge)`. */
  @inline final def -(edge: E): This[N, E] = excl(edge)

  /** Creates a new graph with the elements of this graph minus the passed elements
    * and edges that are incident with any of the passed nodes.
    * Use `diff` to subtract all nodes and edges of another graph.
    *
    * @param edges to be removed.
    * @param isolatedNodes to be removed. Nodes that are implicitly defined by any edge in `edges` will be ignored.
    */
  def removedAll(isolatedNodes: IterableOnce[N], edges: IterableOnce[E]): This[N, E]

  /** Same as `removedAll(isolatedNodes, edges)` but with empty `isolatedNodes`.
    * $edgesOnlyUseCase */
  @inline final def removedAll(edges: IterableOnce[E]): This[N, E] = removedAll(Nil, edges)

  /** Alias for `removedAll(isolatedNodes, edges)`. */
  @inline final def --(isolatedNodes: IterableOnce[N], edges: IterableOnce[E]): This[N, E] =
    removedAll(isolatedNodes, edges)

  /** Alias for `removedAll(edges)`. */
  @inline final def --(edges: IterableOnce[E]): This[N, E] = removedAll(Nil, edges)

  /** Creates a new graph with the elements of this graph minus the elements of `that`
    * and edges that are incident with any node in `that`. */
  def --(that: AnyGraph[N, E]): This[N, E]
}
