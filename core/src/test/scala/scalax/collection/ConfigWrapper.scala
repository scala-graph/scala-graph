package scalax.collection

import scalax.collection.config.GraphConfig
import scalax.collection.generic.{Edge, GenericGraphFactory}

/** Enables to transparently pass `GraphCompanion` objects with non-default configuration parameters to specs.
  */
trait ConfigWrapper[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]] {
  val companion: GenericGraphFactory[CC]
  implicit val config: GraphConfig

  def empty[N, E <: Edge[N]]: CC[N, E] = companion.empty[N, E]

  def apply[N, E[X] <: Edge[X]](elems: OuterElem[N, E[N]]*)(implicit config: GraphConfig): CC[N, E[N]] =
    companion(elems: _*)

  def from[N, E <: Edge[N]](nodes: collection.Iterable[N], edges: collection.Iterable[E])(implicit
      config: GraphConfig
  ): CC[N, E] =
    companion.from[N, E](nodes, edges)

  def from[N, E[X] <: Edge[X]](edges: collection.Iterable[E[N]]): CC[N, E[N]] =
    companion.from[N, E](edges)
}
