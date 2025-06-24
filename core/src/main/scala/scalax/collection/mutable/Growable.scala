package scalax.collection
package mutable

import scala.annotation.unchecked.{uncheckedVariance => uV}

import scalax.collection.generic.Edge

trait Growable[-N, -E <: Edge[N @uV]] {

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
    */
  def addAll(xs: Iterable[OuterElem[N, E]]): this.type = { xs foreach addOuter; this }

  /** Alias for `addAll(xs)`. */
  def ++=(xs: Iterable[OuterElem[N, E]]): this.type = { xs foreach addOuter; this }

  /** Adds all passed nodes and edges to this graph.
    */
  def addAll(nodes: Iterable[N], edges: Iterable[E @uV]): this.type = {
    nodes foreach addOne
    edges foreach +=
    this
  }

  /** Adds all `nodes` to this graph.
    */
  def addNodes(nodes: Iterable[N]): this.type = { nodes foreach addOne; this }

  /** Adds all `edges` to this graph. Nodes being ends of `edges` are also added if not yet present.
    */
  def addEdges(edges: Iterable[E]): this.type = { edges foreach +=; this }
}
