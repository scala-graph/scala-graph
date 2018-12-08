package scalax.collection.mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.higherKinds

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.{OuterEdge, OuterElem, OuterNode}

trait Growable[-N, -E[X] <: EdgeLike[X]] {

  /** Adds a single node to this `Growable`.
    * @return `true` if this `Growable` has not contained `node` before.
    */
  def add(node: N): Boolean

  /** Adds a single node to this `Growable`. */
  def +=(node: N): this.type

  /** Adds a single edge to this `Growable`.
    * @return `true` if this `Growable` has not contained `edge` before
    */
  def add(edge: E[N @uV]): Boolean

  /** Adds a single edge to this `Growable`. */
  def +=(edge: E[N @uV]): this.type

  /** Adds a single outer element to this `Growable`. */
  def addOuter(elem: OuterElem[N, E]): this.type = {
    elem match {
      case OuterNode(n)       => +=(n)
      case e: OuterEdge[N, E] => +=(e.asInstanceOf[E[N]])
    }
    this
  }

  /** Adds all elements produced by `outer` to this `Growable`. */
  def ++=(outer: Iterable[OuterElem[N, E]]): this.type = { outer foreach addOuter; this }

  /** If an inner edge equaling to `edge` is present in this graph, it is replaced
    * by `edge`, otherwise `edge` will be inserted.
    * This is useful if non-key parts of an immutable edge are to be modified.
    * @return `true` if `edge` has been inserted, `false` if it has been replaced.
    */
  def upsert(edge: E[N @uV]): Boolean
}
