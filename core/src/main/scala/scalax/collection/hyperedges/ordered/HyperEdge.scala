package scalax.collection
package hyperedges.ordered

import scalax.collection.generic.{AbstractGenericHyperEdge, HyperEdgeCompanion, HyperEdgeToString, OrderedEndpoints}

/** Undirected hyperedge with ends having sequence semantic with respect to equality.
  */
@SerialVersionUID(-52)
final case class HyperEdge[+N] private (override val ends: Several[N])
    extends AbstractGenericHyperEdge[N, HyperEdge](ends)
    with OrderedEndpoints
    with HyperEdgeToString {

  def map[N](ends: Several[N]): HyperEdge[N] = copy(ends)
}

object HyperEdge extends HyperEdgeCompanion[HyperEdge]
