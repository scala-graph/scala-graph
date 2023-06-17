package scalax.collection.edges

import scalax.collection.generic.{AbstractGenericUnlabeledDiEdge, DiEdgeToString, EdgeCompanion}

/** Represents a generic unlabeled directed edge.
  */
@SerialVersionUID(55)
final case class DiEdge[+N](source: N, target: N) extends AbstractGenericUnlabeledDiEdge[N, DiEdge] with DiEdgeToString

/** Factory for directed edges.
  * See also `DiEdgeImplicits` and `val ~>` in the package object.
  */
object DiEdge extends EdgeCompanion[DiEdge]
