package scalax.collection.edges

import scalax.collection.generic.{AbstractGenericUnDiEdge, EdgeCompanion}

/** Represents a generic unlabeled undirected edge.
  */
@SerialVersionUID(54)
final case class UnDiEdge[+N](node_1: N, node_2: N) extends AbstractGenericUnDiEdge[N, UnDiEdge] {
  validate()
  def map[NN](node_1: NN, node_2: NN): UnDiEdge[NN] = copy[NN](node_1, node_2)
}

/** Factory for undirected edges.
  * See also `UnDiEdgeImplicits` and `val ~` in the package object.
  */
object UnDiEdge extends EdgeCompanion[UnDiEdge]
