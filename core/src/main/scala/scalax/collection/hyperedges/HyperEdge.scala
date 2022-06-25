package scalax.collection.hyperedges

import scala.collection.immutable.Iterable
import scalax.collection.generic.{AnyHyperEdge, HyperEdgeCompanion}

/** Undirected hyperedge with ends having set/bag semantic.
  */
@SerialVersionUID(52)
final case class HyperEdge[+N] private (override val ends: Iterable[N]) extends AnyHyperEdge[N]

object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
  protected def apply[N](ends: Iterable[N]): HyperEdge[N] = new HyperEdge[N](ends)
}
