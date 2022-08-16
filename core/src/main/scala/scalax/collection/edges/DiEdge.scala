package scalax.collection.edges

import scalax.collection.generic.{AbstractGenericDiEdge, DiEdgeToString, EdgeCompanion}

/** Represents a generic unlabeled directed edge.
  */
@SerialVersionUID(55)
final case class DiEdge[+N](source: N, target: N) extends AbstractGenericDiEdge[N, DiEdge] with DiEdgeToString {

  def map[NN](source: NN, target: NN): DiEdge[NN] = copy[NN](source, target)
}

/** Factory for directed edges.
  * See also `DiEdgeImplicits` and `val ~>` in the package object.
  */
object DiEdge extends EdgeCompanion[DiEdge]
