package scalax.collection.edges.multilabeled

import scalax.collection.edges.UnDiEdge
import scalax.collection.generic._

/** Template for generic undirected multiedges with a single `label` field.
  * To support multigraphs, equality is based on the nodes and the `label` field.
  * Mix in `GenericEdgeMapper` to get your derived edge also mappable.
  */
abstract class LUnDiEdge[+N, L] extends AnyUnDiEdge[N] with Label[L] with ExtendedKey with LUnDiEdgeToString {
  protected def labelToString: String = label.toString
  def extendKeyBy: Seq[Any]           = label :: Nil
}

/** Template for an `implicit class` that defines the infix constructor `++` to pass a label like `1 ~> 2 ++ aLabel`.
  */
abstract class LUnDiEdgeInfixConstructor[N, L, CC[X] <: Edge[X] with Label[L] with ExtendedKey](
    apply: (N, N, L) => CC[N]
) {
  def edge: UnDiEdge[N]
  def ++(label: L): CC[N] = apply(edge._1, edge._2, label)
}
