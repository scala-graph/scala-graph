package scalax.collection.mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.higherKinds

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.{OuterEdge, OuterElem, OuterNode}

trait Growable[-N, -E <: EdgeLike[N @uV]] {

  /** Adds a single node to this graph.
    *
    * @return `true` if this graph has not contained `node` before.
    */
  def add(node: N): Boolean

  /** Adds a single node to this graph. */
  def +=(node: N): this.type

  /** Adds a single edge to this graph.
    *
    * @return `true` if this graph has not contained `edge` before
    */
  def add(edge: E): Boolean

  /** Adds a single edge to this graph. */
  def +=(edge: E): this.type

  /** Adds a single outer element to this graph. */
  def addOuter(elem: OuterElem[N, E]): this.type = {
    elem match {
      case n: OuterNode[N]    => +=(n.node)
      case e: OuterEdge[N, E] => +=(e.edge)
    }
    this
  }

  /** Adds all elements produced by `outer` to this graph. */
  def ++=(outer: Iterable[OuterElem[N, E]]): this.type = { outer foreach addOuter; this }

  /** Adds all passed nodes and edges to this graph. */
  def ++=(nodes: Iterable[N] = Nil, edges: Iterable[E @uV] = Nil): this.type = {
    nodes foreach +=
    edges foreach +=
    this
  }
}
