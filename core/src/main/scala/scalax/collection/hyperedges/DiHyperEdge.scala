package scalax.collection.hyperedges

import scalax.collection.generic.{AnyDiHyperEdge, DiHyperEdgeCompanion, OrderedEndpoints}

/** Represents a directed edge in a hypergraph with an unlimited number of source and of target nodes.
  * Target nodes are handled as a $BAG.
  */
@SerialVersionUID(53)
final case class DiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N])
    extends AnyDiHyperEdge[N] {
  validate()
}
object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge]

/** Represents a directed edge in a hypergraph with an unlimited number of source and of target nodes
  * where sources and targets are handled as an ordered sequence.
  */
@SerialVersionUID(-53)
final case class OrderedDiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N])
    extends AnyDiHyperEdge[N]
    with OrderedEndpoints {
  validate()
}
object OrderedDiHyperEdge extends DiHyperEdgeCompanion[OrderedDiHyperEdge]
