package scalax.collection.edges

import scalax.collection.generic.{AbstractGenericUnlabeledUnDiEdge, EdgeCompanion, UnDiEdgeToString}

/** Represents a generic unlabeled undirected edge.
  */
@SerialVersionUID(54)
final case class UnDiEdge[+N](source: N, target: N)
    extends AbstractGenericUnlabeledUnDiEdge[N, UnDiEdge]
    with UnDiEdgeToString

/** Factory for undirected edges.
  * See also `UnDiEdgeImplicits` and `val ~` in the package object.
  */
object UnDiEdge extends EdgeCompanion[UnDiEdge]
