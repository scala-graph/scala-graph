package scalax.collection.mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.higherKinds

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.{OuterEdge, OuterElem, OuterNode}

trait Shrinkable[-N, -E[X] <: EdgeLike[X]] {

  /** Adds a single node to this `Growable`. */
  def -=(node: N): this.type

  /** Adds a single edge to this `Growable`. */
  def -=(edge: E[N @uV]): this.type

  /** Adds all elements produced by `outer` to this `Growable`. */
  def --=(outer: Iterable[OuterElem[N, E]]): this.type = {
    val it = outer.iterator
    while (it.hasNext) it.next() match {
      case OuterNode(n)       => -=(n)
      case e: OuterEdge[N, E] => -=(e.asInstanceOf[E[N]])
    }
    this
  }
}
