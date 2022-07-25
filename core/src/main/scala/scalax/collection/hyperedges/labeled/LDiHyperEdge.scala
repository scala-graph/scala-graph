package scalax.collection
package hyperedges.labeled

import scalax.collection.generic.{AbstractDiHyperEdge, LDiHyperEdgeToString, SingleLabel}
import scalax.collection.hyperedges.DiHyperEdge

/** Template for generic directed hyperedges with a single `label` field.
  * Equality is based solely on the `ends` so this trait is not suitable for multigraphs.
  * Mix in `GenericDiHyperEdgeMapper` to get your derived hyperedge also mappable.
  */
abstract class LDiHyperEdge[+N, L](sources: OneOrMore[N], targets: OneOrMore[N])
    extends AbstractDiHyperEdge[N](sources, targets)
    with SingleLabel[L]
    with LDiHyperEdgeToString

/** Template for an `implicit class` that defines the infix constructor `+` to pass a label like `Several(1, 2) ~~> One(3) + aLabel`.
  */
abstract class LDiHyperEdgeInfixConstructor[N, L, CC[X] <: AbstractDiHyperEdge[X] with SingleLabel[L]](
    apply: (OneOrMore[N], OneOrMore[N], L) => CC[N]
) {
  def diHyperedge: DiHyperEdge[N]
  def +(label: L): CC[N] = apply(diHyperedge.sources, diHyperedge.targets, label)
}
