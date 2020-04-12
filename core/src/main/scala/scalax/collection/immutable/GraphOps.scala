package scalax.collection.immutable

import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphLike

trait GraphOps[N, E <: EdgeLike[N], +This[X, Y <: EdgeLike[X]] <: GraphLike[X, Y, This] with Graph[X, Y]] {

  /** Creates a new supergraph with an additional node unless this graph contains `node`. */
  def +(node: N): This[N, E]

  /** Creates a new supergraph with an additional edge unless this graph contains `edge`. */
  def +(edge: E): This[N, E]

  /** Creates a new graph with the elements of this graph minus `node` and its incident edges. */
  def -(node: N): This[N, E]

  /** Creates a new graph with the elements of this graph minus `edge`. */
  def -(edge: E): This[N, E]

  /** Creates a new graph with the elements of this graph minus the passed elements
    * and edges that are incident with any of the passed nodes. */
  def --(nodes: Iterable[N] = Nil, edges: Iterable[E] = Nil): This[N, E]

  /** Creates a new graph with the elements of this graph minus the elements of `that`
    * and edges that are incident with any node in `that`. */
  def --(that: AnyGraph[N, E]): This[N, E]
}
