package scalax.collection.hyperedges

import scalax.collection.generic.{AbstractHyperEdge, Ends, HyperEdgeCompanion, HyperEdgeToString}

/** Undirected hyperedge with ends having set/bag semantic.
  */
@SerialVersionUID(52)
final case class HyperEdge[+N](override val ends: Ends[N]) extends AbstractHyperEdge[N](ends) with HyperEdgeToString

object HyperEdge extends HyperEdgeCompanion[HyperEdge]
