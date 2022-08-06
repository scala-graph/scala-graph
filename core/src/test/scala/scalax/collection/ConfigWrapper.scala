package scalax.collection

import scalax.collection.generic._
import scalax.collection.generic.GraphCompanion

/** Enables to transparently pass `GraphCompanion` objects with non-default configuration parameters to specs.
  */
trait ConfigWrapper[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]] {
  val companion: GraphCompanion[CC]
  implicit val config: companion.Config

  def empty[N, E <: Edge[N]](implicit config: companion.Config): CC[N, E] = companion.empty[N, E]

  def apply[N, E[X] <: Edge[X]](elems: OuterElem[N, E[N]]*)(implicit config: companion.Config) =
    companion(elems: _*)

  def from[N, E <: Edge[N]](nodes: collection.Iterable[N], edges: collection.Iterable[E])(implicit
      config: companion.Config
  ) =
    companion.from[N, E](nodes, edges)

  def from[N, E[X] <: Edge[X]](edges: collection.Iterable[E[N]]) = companion.from[N, E](edges)
}
