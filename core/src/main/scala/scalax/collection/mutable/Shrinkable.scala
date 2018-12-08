package scalax.collection.mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.higherKinds

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.{OuterEdge, OuterElem, OuterNode}

trait Shrinkable[-N, -E[X] <: EdgeLike[X]] {

  /** Removes a single node from this `Shrinkable`.
    * @return whether the node existed before
    */
  def remove(node: N): Boolean

  /** Removes a single node from this `Shrinkable`. */
  def -=(node: N): this.type

  /** Removes a single edge from this `Shrinkable`.
    * @return whether the edge existed before
    */
  def remove(edge: E[N @uV]): Boolean

  /** Removes a single edge from this `Shrinkable`. */
  def -=(edge: E[N @uV]): this.type

  /** Removes all elements produced by `outer` from this `Shrinkable`. */
  def --=(outer: Iterable[OuterElem[N, E]]): this.type = {
    val it = outer.iterator
    while (it.hasNext) it.next() match {
      case OuterNode(n)       => -=(n)
      case e: OuterEdge[N, E] => -=(e.asInstanceOf[E[N]])
    }
    this
  }

  /** Shrinks this graph to its intersection with the `outer` elements. */
  def &=(outer: Iterable[OuterElem[N, E]]): this.type
}
