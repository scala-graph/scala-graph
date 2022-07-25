package scalax.collection.hyperedges.ordered

import scalax.collection.generic.{AbstractHyperEdge, HyperEdgeCompanion, HyperEdgeToString, OrderedEndpoints}

/** Undirected hyperedge with ends having sequence semantic with respect to equality.
  */
@SerialVersionUID(-52)
final case class HyperEdge[+N] private (override val ends: Several[N])
    extends AbstractHyperEdge[N](ends)
    with OrderedEndpoints
    with HyperEdgeToString

object HyperEdge extends HyperEdgeCompanion[HyperEdge]
