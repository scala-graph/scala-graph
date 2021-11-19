package scalax.collection
package mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}

import scala.collection.compat._
import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike

trait Shrinkable[-N, -E <: EdgeLike[N @uV]] extends OuterElems[N @uV, E @uV] {

  /** Removes a single node from this graph.
    * @return whether the node existed before.
    */
  def remove(node: N): Boolean

  /** Removes a single node from this graph. */
  def subtractOne(node: N): this.type

  /** Alias for `subtractOne(node)`. */
  @inline final def -=(node: N): this.type = subtractOne(node)

  /** Removes a single edge from this graph.
    *
    * @return whether the edge existed before
    */
  def remove(edge: E): Boolean

  /** Removes a single edge from this graph. */
  def subtractOne(edge: E): this.type

  /** Alias for `subtractOne(node)`. */
  @inline final def -=(edge: E): this.type = subtractOne(edge)

  /** Removes all `nodes` and `edges` from this graph. */
  final def removeAll(nodes: IterableOnce[N @uV], edges: IterableOnce[E @uV]): this.type = {
    nodes.iterator.foreach(-=)
    removeAll(edges)
  }

  /** Removes all `edges` from this graph. */
  final def removeAll(edges: IterableOnce[E @uV]): this.type = {
    edges.iterator.foreach(-=)
    this
  }

  /** Alias for `removeAll(nodes, edges)`. */
  @inline final def --=(nodes: IterableOnce[N @uV], edges: IterableOnce[E @uV]): this.type =
    removeAll(nodes, edges)

  /** Alias for `removeAll(edges)`. */
  @inline final def --=(edges: IterableOnce[E @uV]): this.type =
    removeAll(edges)

  /** Removes all nodes and edges contained in `that` graph from this graph. */
  final def diffInPlace(that: AnyGraph[N @uV, E @uV]): this.type = {
    that.nodes.outerIterator foreach -=
    that.edges.outerIterator foreach -=
    this
  }

  /** Alias for `diffInPlace`. */
  @inline final def --=(that: AnyGraph[N @uV, E @uV]): this.type = diffInPlace(that)
}
