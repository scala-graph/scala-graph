package scalax.collection
package hyperedges

import scalax.collection.generic.{AbstractGenericUnlabeledDiHyperEdge, DiHyperEdgeCompanion, DiHyperEdgeToString}

/** Directed hyperedge with sources and targets having set/bag semantic each.
  */
@SerialVersionUID(53)
final case class DiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N])
    extends AbstractGenericUnlabeledDiHyperEdge[N, DiHyperEdge](sources, targets)
    with DiHyperEdgeToString

object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge]
