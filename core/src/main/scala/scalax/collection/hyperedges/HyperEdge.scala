package scalax.collection
package hyperedges

import scalax.collection.generic.{AbstractUnlabeledGenericHyperEdge, HyperEdgeCompanion, HyperEdgeToString}

/** Undirected hyperedge with ends having set/bag semantic.
  */
@SerialVersionUID(52)
final case class HyperEdge[+N](override val ends: Several[N])
    extends AbstractUnlabeledGenericHyperEdge[N, HyperEdge](ends)
    with HyperEdgeToString

object HyperEdge extends HyperEdgeCompanion[HyperEdge]
