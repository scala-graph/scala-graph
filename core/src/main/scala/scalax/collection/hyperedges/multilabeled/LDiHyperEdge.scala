package scalax.collection
package hyperedges.multilabeled

import scalax.collection.generic.{
  AbstractDiHyperEdge, ExtendedKeyBySingleLabel, LDiHyperEdgeToString, MultiEdge, MultiLEdgeToString, SingleLabel
}
import scalax.collection.hyperedges.DiHyperEdge

/** Template for generic directed multi-hyperedges with a single `label` field.
  * To support multigraphs, equality is based on `sources`, `targets` and the `label` field.
  * Mix in `GenericDiHyperEdgeMapper` to get your derived multi-hyperedge also mappable.
  */
abstract class LDiHyperEdge[+N, L](sources: OneOrMore[N], targets: OneOrMore[N])
    extends AbstractDiHyperEdge[N](sources, targets)
    with SingleLabel[L]
    with ExtendedKeyBySingleLabel
    with LDiHyperEdgeToString
    with MultiLEdgeToString

/** Template for an `implicit class` that defines the infix constructor `++` to pass a label like
  * `Several(1, 2) ~~> One(3) ++ aLabel`.
  */
abstract class LDiHyperEdgeInfixConstructor[N, L, CC[X] <: AbstractDiHyperEdge[X] with MultiEdge](
    apply: (OneOrMore[N], OneOrMore[N], L) => CC[N]
) {
  def diHyperedge: DiHyperEdge[N]
  def ++(label: L): CC[N] = apply(diHyperedge.sources, diHyperedge.targets, label)
}
