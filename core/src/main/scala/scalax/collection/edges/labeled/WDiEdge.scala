package scalax.collection.edges.labeled

import scalax.collection.generic.{AbstractGenericDiEdge, WDiEdgeToString}

/** Generic weighted directed edge.
  */
@SerialVersionUID(54)
final case class WDiEdge[+N](source: N, target: N, override val weight: Double)
    extends AbstractGenericDiEdge[N, WDiEdge]
    with WDiEdgeToString {

  def map[NN](source: NN, target: NN): WDiEdge[NN] = copy[NN](source, target)
}

/** Infix extractor for weighted directed edges to be combined with `%` like `case a :~> b % w`.
  */
object :~> {
  def unapply[N](e: WDiEdge[N]): Some[(N, (N, Double))] = Some(e.source, (e.target, e.weight))
}
