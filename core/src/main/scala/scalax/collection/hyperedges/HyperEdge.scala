package scalax.collection.hyperedges

import scalax.collection.generic.{AnyHyperEdge, HyperEdgeCompanion, OrderedEndpoints}

/** Represents an undirected hyperedge (hyperlink) with ends of bag semantic. */
@SerialVersionUID(52)
final case class HyperEdge[+N](override val ends: Iterable[N]) extends AnyHyperEdge[N]

object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
  protected def from[N](ends: Iterable[N]): HyperEdge[N] = new HyperEdge[N](ends)
}

@SerialVersionUID(-52)
final case class OrderedHyperEdge[+N](override val ends: Iterable[N]) extends AnyHyperEdge[N] with OrderedEndpoints

object OrderedHyperEdge extends HyperEdgeCompanion[OrderedHyperEdge] {
  protected def from[N](ends: Iterable[N]): OrderedHyperEdge[N] = new OrderedHyperEdge[N](ends)
}
