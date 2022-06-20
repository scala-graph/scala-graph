package scalax.collection.hyperedges.ordered

import scalax.collection.generic.{AnyHyperEdge, HyperEdgeCompanion, OrderedEndpoints}

/** Undirected hyperedge with ends having sequence semantic.
  */
@SerialVersionUID(-52)
final case class HyperEdge[+N](override val ends: Iterable[N]) extends AnyHyperEdge[N] with OrderedEndpoints

object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
  protected def from[N](ends: Iterable[N]): HyperEdge[N] = new HyperEdge[N](ends)
}
