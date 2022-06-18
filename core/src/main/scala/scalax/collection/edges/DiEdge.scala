package scalax.collection.edges

import scalax.collection.generic.{AbstractGenericDiEdge, EdgeCompanion}

/** Represents a generic unlabeled directed edge.
  */
@SerialVersionUID(55)
final case class DiEdge[+N](source: N, target: N) extends AbstractGenericDiEdge[N, DiEdge] {

  def map[NN](node_1: NN, node_2: NN): DiEdge[NN] = copy[NN](node_1, node_2)
}

/** Factory for directed edges.
  * See also `DiEdgeImplicits` and `val ~>` in the package object.
  */
object DiEdge extends EdgeCompanion[DiEdge]
