package scalax.collection
package hyperedges

import scalax.collection.generic.{AbstractHyperEdge, HyperEdgeCompanion, HyperEdgeToString}

/** Undirected hyperedge with ends having set/bag semantic.
  */
@SerialVersionUID(52)
final case class HyperEdge[+N](override val ends: Several[N]) extends AbstractHyperEdge[N](ends) with HyperEdgeToString

object HyperEdge extends HyperEdgeCompanion[HyperEdge]
