package scalax.collection.hyperedges

import scala.collection.immutable.Iterable
import scalax.collection.generic.{AnyDiHyperEdge, DiHyperEdgeCompanion}

/** Directed hyperedge with sources and targets having set/bag semantic each.
  */
@SerialVersionUID(53)
final case class DiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N])
    extends AnyDiHyperEdge[N]

object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge]
