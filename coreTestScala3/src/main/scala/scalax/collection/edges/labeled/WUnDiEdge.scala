package scalax.collection.edges.labeled

import scalax.collection.generic.{AbstractGenericUnDiEdge, WUnDiEdgeToString}

/** Generic weighted undirected edge.
  */
@SerialVersionUID(54)
final case class WUnDiEdge[+N](source: N, target: N, override val weight: Double)
    extends AbstractGenericUnDiEdge[N, WUnDiEdge]
    with WUnDiEdgeToString {

  def map[NN](node_1: NN, node_2: NN): WUnDiEdge[NN] = copy[NN](node_1, node_2)
}

/** Infix extractor for weighted undirected edges to be combined with `%` like `case a :~ b % w`.
  */
object :~ {
  def unapply[N](e: WUnDiEdge[N]): Some[(N, (N, Double))] = Some(e.node1, (e.node2, e.weight))
}
