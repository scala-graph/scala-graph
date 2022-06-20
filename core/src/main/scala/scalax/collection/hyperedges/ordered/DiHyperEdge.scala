package scalax.collection.hyperedges.ordered

import scalax.collection.generic.{AnyDiHyperEdge, DiHyperEdgeCompanion, OrderedEndpoints}

/** Directed hyperedge with sources and ends having sequence semantic each.
  */
@SerialVersionUID(-53)
final case class DiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N])
    extends AnyDiHyperEdge[N]
    with OrderedEndpoints

object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge]
