package scalax.collection
package hyperedges.ordered

import scalax.collection.generic.{
  AbstractGenericDiHyperEdge, DiHyperEdgeCompanion, DiHyperEdgeToString, OrderedEndpoints
}

/** Directed hyperedge with sources and targets having sequence semantic each.
  */
@SerialVersionUID(-53)
final case class DiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N])
    extends AbstractGenericDiHyperEdge[N, DiHyperEdge](sources, targets)
    with OrderedEndpoints
    with DiHyperEdgeToString {

  def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): DiHyperEdge[N] = DiHyperEdge(sources, targets)
}

object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge]
