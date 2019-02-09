package scalax.collection.mutable

import scala.collection.mutable.Cloneable
import scala.language.higherKinds

import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike

trait GraphOps[N, E <: EdgeLike[N], +This[X, Y <: EdgeLike[X]] <: GraphLike[X, Y, This] with Graph[X, Y]]
    extends Growable[N, E]
    with Shrinkable[N, E]
    with AbstractBuilder[N, E]
    with Cloneable[This[N, E]] {

  /** Adds all elements in `other` to this `Growable`. */
  def ++=(other: AnyGraph[N, E]): this.type = ???

  /** Shrinks this graph to its intersection with the `outer` elements. */
  def &=(nodes: Iterable[N] = Nil, edges: Iterable[E] = Nil): this.type = {
    nodes foreach -=
    edges foreach -=
    this
  }
}
