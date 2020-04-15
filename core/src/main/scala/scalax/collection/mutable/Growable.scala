package scalax.collection.mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.{OuterEdge, OuterElem, OuterNode}

trait Growable[-N, -E <: EdgeLike[N @uV]] {

  /** Adds a single node to this graph.
    * @return `true` if this graph has not contained `node` before.
    */
  protected def add(node: N): Boolean

  /** Adds a single node to this graph. */
  def addOne(node: N): this.type = { add(node); this }

  /** Alias for `addOne(node)`. */
  @inline final def +=(node: N): this.type = addOne(node)

  /** Adds a single edge to this graph.
    * @return `true` if this graph has not contained `edge` before.
    */
  protected def add(edge: E): Boolean

  /** Adds a single node to this graph. */
  def addOne(edge: E): this.type = { add(edge); this }

  /** Alias for `addOne(edge)`. */
  @inline final def +=(edge: E): this.type = addOne(edge)

  /** Adds a single outer element to this graph. */
  final protected[collection] def addOuter(elem: OuterElem[N, E]): this.type = {
    elem match {
      case n: OuterNode[N]    => addOne(n.node)
      case e: OuterEdge[N, E] => +=(e.edge)
    }
    this
  }

  /** Adds all elements produced by `outer` to this graph.
    * For a graph see also `unionInPlace`.
    */
  def addAll(xs: Iterable[OuterElem[N, E]]): this.type = { xs foreach addOuter; this }

  /** Alias for `addAll(xs)`. */
  def ++=(xs: Iterable[OuterElem[N, E]]): this.type = { xs foreach addOuter; this }

  /** Adds all passed nodes and edges to this graph.
    * For a graph see also `unionInPlace`.
    */
  def ++=(nodes: Iterable[N] = Nil, edges: Iterable[E @uV] = Nil): this.type = {
    nodes foreach addOne
    edges foreach +=
    this
  }
}
