package scalax.collection
package hyperedges
package ordered.multilabeled

import scalax.collection.generic.{
  AbstractDiHyperEdge, ExtendedKeyBySingleLabel, LDiHyperEdgeToString, MultiLEdgeToString, OrderedEndpoints, SingleLabel
}

/** Template for generic directed, ordered multi-hyperedges with a single `label` field.
  * To support multigraphs, equality is based on `sources`, `targets` and the `label` field.
  * Ordered means that `sources` and `targets` have sequence semantic with respect to equality.
  * Mix in `GenericDiHyperEdgeMapper` to get your derived hyperedge also mappable.
  */
abstract class LDiHyperEdge[+N, L](sources: OneOrMore[N], targets: OneOrMore[N])
    extends AbstractDiHyperEdge[N](sources, targets)
    with OrderedEndpoints
    with SingleLabel[L]
    with ExtendedKeyBySingleLabel
    with LDiHyperEdgeToString
    with MultiLEdgeToString
