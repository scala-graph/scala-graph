package scalax.collection.edges.multilabeled

import scalax.collection.generic.{AbstractGenericUnDiEdge, ExtendedKeyByWeight, MultiWEdgeToString, WUnDiEdgeToString}

/** Generic weighted undirected edge for multigraphs.
  */
@SerialVersionUID(54)
final case class WUnDiEdge[+N](source: N, target: N, override val weight: Double)
    extends AbstractGenericUnDiEdge[N, WUnDiEdge]
    with ExtendedKeyByWeight
    with WUnDiEdgeToString
    with MultiWEdgeToString {

  def map[NN](node_1: NN, node_2: NN): WUnDiEdge[NN] = copy[NN](node_1, node_2)
}

/** Infix extractor for weighted undirected edges in multigraphs to be combined with `%%` like `case a ::~ b % w`.
  */
object ::~ {
  def unapply[N](e: WUnDiEdge[N]): Option[(N, (N, Double))] = Some(e._1, (e._2, e.weight))
}
