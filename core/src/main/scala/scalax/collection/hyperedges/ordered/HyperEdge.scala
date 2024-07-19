package scalax.collection
package hyperedges.ordered

import scalax.collection.generic.{
  AbstractUnlabeledGenericHyperEdge, HyperEdgeCompanion, HyperEdgeToString, OrderedEndpoints
}

/** Undirected hyperedge with ends having sequence semantic with respect to equality.
  */
@SerialVersionUID(-52)
final case class HyperEdge[+N](override val ends: Several[N])
    extends AbstractUnlabeledGenericHyperEdge[N, HyperEdge](ends)
    with OrderedEndpoints
    with HyperEdgeToString

object HyperEdge extends HyperEdgeCompanion[HyperEdge]
