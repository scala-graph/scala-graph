package scalax.collection.edges.labeled

import scalax.collection.edges.DiEdge
import scalax.collection.generic.{AnyDiEdge, Edge, LDiEdgeToString, SingleLabel}

/** Template for generic directed edges with a single `label` field.
  * Equality is based solely on the `ends` so this trait is not suitable for multigraphs.
  * Mix in `GenericEdgeMapper` to get your derived edge also mappable.
  */
abstract class LDiEdge[+N, L] extends AnyDiEdge[N] with SingleLabel[L] with LDiEdgeToString

/** Template for an `implicit class` that defines the infix constructor `+` to pass a label like `1 ~> 2 + aLabel`.
  */
abstract class LDiEdgeInfixConstructor[N, L, CC[X] <: Edge[X] with SingleLabel[L]](apply: (N, N, L) => CC[N]) {
  def edge: DiEdge[N]
  def +(label: L): CC[N] = apply(edge.source, edge.target, label)
}
