package scalax.collection
package mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.higherKinds

import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike

trait Shrinkable[-N, -E <: EdgeLike[N @uV]] extends OuterElems[N @uV, E @uV] {

  /** Removes a single node from this graph.
    *
    * @return whether the node existed before
    */
  def remove(node: N): Boolean

  /** Removes a single node from this graph. */
  def -=(node: N): this.type

  /** Removes a single edge from this graph.
    *
    * @return whether the edge existed before
    */
  def remove(edge: E): Boolean

  /** Removes a single edge from this graph. */
  def -=(edge: E): this.type

  /** Removes all elements produced by `outer` from this graph. */
  final def --=(nodes: Iterable[N] = Nil, edges: Iterable[E @uV] = Nil): this.type = {
    nodes foreach -=
    edges foreach -=
    this
  }

  /** Removes all nodes and edges contained in `other` from this graph. */
  final def --=(that: AnyGraph[N @uV, E @uV]): this.type = {
    that.nodes.toOuter foreach -=
    that.edges.toOuter foreach -=
    this
  }
}
