package scalax.collection
package hyperedges

import scalax.collection.generic.{AbstractGenericHyperEdge, HyperEdgeCompanion, HyperEdgeToString}

/** Undirected hyperedge with ends having set/bag semantic.
  */
@SerialVersionUID(52)
final case class HyperEdge[+N](override val ends: Several[N])
    extends AbstractGenericHyperEdge[N, HyperEdge](ends)
    with HyperEdgeToString {

  def map[N](ends: Several[N]): HyperEdge[N] = copy(ends)
}

object HyperEdge extends HyperEdgeCompanion[HyperEdge]
