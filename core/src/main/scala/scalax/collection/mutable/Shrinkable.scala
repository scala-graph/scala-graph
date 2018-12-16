package scalax.collection
package mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.higherKinds

import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike

trait Shrinkable[-N, -E[X] <: EdgeLike[X]] extends OuterElems[N @uV, E @uV] {

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
  def remove(edge: E[N @uV]): Boolean

  /** Removes a single edge from this graph. */
  def -=(edge: E[N @uV]): this.type

  /** Removes all elements produced by `outer` from this graph. */
  def --=(outer: Iterable[OuterElem]): this.type = {
    val it = outer.iterator
    while (it.hasNext) it.next() match {
      case OuterNode(n) => -=(n)
      case OuterEdge(e) => -=(e)
    }
    this
  }

  /** Removes all nodes and edges contained in `other` from this graph. */
  def --=(other: AnyGraph[N @uV, E @uV]): this.type = ???
}
