package scalax.collection.edges.labeled

import scalax.collection.edges.UnDiEdge
import scalax.collection.generic.{AnyUnDiEdge, Edge, LUnDiEdgeToString, Label}

/** Template for generic undirected edges with a single `label` field.
  * Equality is based solely on the nodes so this trait is not suitable for multigraphs.
  * Mix in `GenericEdgeMapper` to get your derived edge also mappable.
  */
abstract class LUnDiEdge[+N, L] extends AnyUnDiEdge[N] with Label[L] with LUnDiEdgeToString {
  protected def labelToString: String = label.toString
}

/** Template for an `implicit class` that defines the infix constructor `+` to pass a label like `1 ~> 2 + aLabel`.
  */
abstract class LUnDiEdgeInfixConstructor[N, L, CC[X] <: Edge[X] with Label[L]](apply: (N, N, L) => CC[N]) {
  def edge: UnDiEdge[N]
  def +(label: L): CC[N] = apply(edge._1, edge._2, label)
}
