package scalax.collection
package hyperedges.ordered

import scalax.collection.generic.{
  AbstractGenericUnlabeledDiHyperEdge, DiHyperEdgeCompanion, DiHyperEdgeToString, OrderedEndpoints
}

/** Directed hyperedge with sources and targets having sequence semantic each.
  */
@SerialVersionUID(-53)
final case class DiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N])
    extends AbstractGenericUnlabeledDiHyperEdge[N, DiHyperEdge](sources, targets)
    with OrderedEndpoints
    with DiHyperEdgeToString

object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge]
