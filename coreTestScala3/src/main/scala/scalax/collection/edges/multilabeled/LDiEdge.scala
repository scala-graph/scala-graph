package scalax.collection.edges.multilabeled

import scalax.collection.edges.DiEdge
import scalax.collection.generic.{
  AnyDiEdge, Edge, ExtendedKeyBySingleLabel, LDiEdgeToString, MultiEdge, MultiLEdgeToString, SingleLabel
}

/** Template for generic directed multiedges with a single `label` field.
  * To support multigraphs, equality is based on the `nodes` and the `label` field.
  * Mix in `GenericEdgeMapper` to get your derived edge also mappable.
  */
abstract class LDiEdge[+N, L]
    extends AnyDiEdge[N]
    with SingleLabel[L]
    with ExtendedKeyBySingleLabel
    with LDiEdgeToString
    with MultiLEdgeToString

/** Template for an `implicit class` that defines the infix constructor `++` to pass a label like `1 ~> 2 ++ aLabel`.
  */
abstract class LDiEdgeInfixConstructor[N, L, CC[X] <: Edge[X] with MultiEdge](
    apply: (N, N, L) => CC[N]
) {
  def edge: DiEdge[N]
  def :++(label: L): CC[N] = apply(edge.source, edge.target, label)
}
