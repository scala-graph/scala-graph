package scalax.collection.edges.multilabeled

import scalax.collection.generic.{AbstractGenericDiEdge, ExtendedKeyByWeight, MultiWEdgeToString, WDiEdgeToString}

/** Generic weighted undirected edge for multigraphs.
  */
@SerialVersionUID(54)
final case class WDiEdge[+N](source: N, target: N, override val weight: Double)
    extends AbstractGenericDiEdge[N, WDiEdge]
    with ExtendedKeyByWeight
    with WDiEdgeToString
    with MultiWEdgeToString {

  def map[NN](source: NN, target: NN): WDiEdge[NN] = copy[NN](source, target)
}

/** Infix extractor for weighted directed edges in multigraphs to be combined with `%%` like `case a ::~> b %% w`.
  */
object ::~> {
  def unapply[N](e: WDiEdge[N]): Some[(N, (N, Double))] = Some(e.source, (e.target, e.weight))
}
