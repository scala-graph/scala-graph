package scalax.collection.hyperedges.ordered

import scala.collection.immutable.Iterable
import scalax.collection.generic.{AnyDiHyperEdge, DiHyperEdgeCompanion, DiHyperEdgeToString, OrderedEndpoints}

/** Directed hyperedge with sources and ends having sequence semantic each.
  */
@SerialVersionUID(-53)
final case class DiHyperEdge[+N] private (override val sources: Iterable[N], override val targets: Iterable[N])
    extends AnyDiHyperEdge[N]
    with OrderedEndpoints
    with DiHyperEdgeToString

object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge] {
  protected def apply[N](sources: Iterable[N], targets: Iterable[N]): DiHyperEdge[N] =
    new DiHyperEdge[N](sources, targets)
}
