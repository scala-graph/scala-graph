package scalax.collection.hyperedges.ordered

import scalax.collection.generic.{AbstractHyperEdge, Ends, HyperEdgeCompanion, HyperEdgeToString, OrderedEndpoints}

/** Undirected hyperedge with ends having sequence semantic with respect to equality.
  */
@SerialVersionUID(-52)
final case class HyperEdge[+N] private (override val ends: Ends[N])
    extends AbstractHyperEdge[N](ends)
    with OrderedEndpoints
    with HyperEdgeToString

object HyperEdge extends HyperEdgeCompanion[HyperEdge]
