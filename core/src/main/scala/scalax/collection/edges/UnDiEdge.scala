package scalax.collection.edges

import scalax.collection.generic.{AbstractGenericUnDiEdge, EdgeCompanion, UnDiEdgeToString}

/** Represents a generic unlabeled undirected edge.
  */
@SerialVersionUID(54)
final case class UnDiEdge[+N](source: N, target: N) extends AbstractGenericUnDiEdge[N, UnDiEdge] with UnDiEdgeToString {

  def map[NN](node_1: NN, node_2: NN): UnDiEdge[NN] = copy[NN](node_1, node_2)
}

/** Factory for undirected edges.
  * See also `UnDiEdgeImplicits` and `val ~` in the package object.
  */
object UnDiEdge extends EdgeCompanion[UnDiEdge]
