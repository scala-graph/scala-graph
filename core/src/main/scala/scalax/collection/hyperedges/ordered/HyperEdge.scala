package scalax.collection.hyperedges.ordered

import scala.collection.immutable.Iterable
import scalax.collection.generic.{AnyHyperEdge, HyperEdgeCompanion, OrderedEndpoints}

/** Undirected hyperedge with ends having sequence semantic.
  */
@SerialVersionUID(-52)
final case class HyperEdge[+N] private (override val ends: Iterable[N]) extends AnyHyperEdge[N] with OrderedEndpoints

object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
  protected def apply[N](ends: Iterable[N]): HyperEdge[N] = new HyperEdge[N](ends)
}
