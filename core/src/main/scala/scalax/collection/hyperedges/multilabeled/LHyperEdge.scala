package scalax.collection
package hyperedges.multilabeled

import scalax.collection.generic.{
  AbstractHyperEdge, ExtendedKeyBySingleLabel, LHyperEdgeToString, MultiEdge, MultiLEdgeToString, SingleLabel
}
import scalax.collection.hyperedges.HyperEdge

/** Template for generic undirected multi-hyperedges with a single `label` field.
  * To support multigraphs, equality is based on `ends` and the `label` field.
  * Mix in `GenericHyperedgeMapper` to get your derived multi-hyperedge also mappable.
  */
abstract class LHyperEdge[+N, L](ends: Several[N])
    extends AbstractHyperEdge[N](ends)
    with SingleLabel[L]
    with ExtendedKeyBySingleLabel
    with LHyperEdgeToString
    with MultiLEdgeToString

/** Template for an `implicit class` that defines the infix constructor `++` to pass a label like
  * `1 ~~ 2 ~~ 3 ++ aLabel`.
  */
abstract class LHyperEdgeInfixConstructor[N, L, CC[X] <: AbstractHyperEdge[X] with MultiEdge](
    apply: (Several[N], L) => CC[N]
) {
  def hyperedge: HyperEdge[N]
  def :++(label: L): CC[N] = apply(hyperedge.ends, label)
}
