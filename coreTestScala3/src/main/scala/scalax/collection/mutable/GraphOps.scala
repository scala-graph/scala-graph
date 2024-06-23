package scalax.collection.mutable

import scala.collection.mutable.Cloneable

import scalax.collection.{AnyGraph => AnyGraph, GraphOps => AnyGraphOps}
import scalax.collection.generic.Edge

trait GraphOps[N, E <: Edge[N], +CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with Graph[X, Y]]
    extends Growable[N, E]
    with Shrinkable[N, E]
    with AbstractBuilder[N, E]
    with Cloneable[CC[N, E]] { this: AnyGraphOps[N, E, CC] =>

  def add(node: N): Boolean
  def add(edge: E): Boolean

  /** Adds all elements in `other` to this `Growable`. */
  final def unionInPlace(that: AnyGraph[N, E]): this.type = {
    that.nodes foreach (this += _.outer)
    that.edges foreach (this += _.outer)
    this
  }

  /** Alias for `unionInPlace`. */
  @inline final def |=(that: AnyGraph[N, E]): this.type = unionInPlace(that)

  /** Computes a new graph with nodes satisfying `fNode` and edges satisfying `fEdge`.
    * If both `fNode` and `fEdge` have default values the original graph is retained.
    */
  def filterInPlace(fNode: NodePredicate = anyNode, fEdge: EdgePredicate = anyEdge): this.type

  /** Shrinks this graph to its intersection with `that` graph. */
  final def intersectInPlace(that: AnyGraph[N, E]): this.type =
    filterInPlace(n => that(n.outer), e => that(e.outer))

  /** Alias for `intersectInPlace`. */
  @inline final def &=(that: AnyGraph[N, E]): this.type = intersectInPlace(that)
}
