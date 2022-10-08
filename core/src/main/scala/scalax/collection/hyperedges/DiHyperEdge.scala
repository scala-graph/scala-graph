package scalax.collection
package hyperedges

import scalax.collection.generic.{AbstractGenericDiHyperEdge, DiHyperEdgeCompanion, DiHyperEdgeToString}

/** Directed hyperedge with sources and targets having set/bag semantic each.
  */
@SerialVersionUID(53)
final case class DiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N])
    extends AbstractGenericDiHyperEdge[N, DiHyperEdge](sources, targets)
    with DiHyperEdgeToString {

  def map[N](sources: OneOrMore[N], targets: OneOrMore[N]): DiHyperEdge[N] = DiHyperEdge(sources, targets)
}

object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge]
