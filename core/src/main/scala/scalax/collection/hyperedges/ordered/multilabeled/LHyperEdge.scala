package scalax.collection
package hyperedges
package ordered.multilabeled

import scalax.collection.generic.{
  AbstractHyperEdge, ExtendedKey, ExtendedKeyBySingleLabel, LHyperEdgeToString, MultiLEdgeToString, OrderedEndpoints,
  SingleLabel
}

/** Template for generic undirected multi-hyperedges with ordered `ends` and a single `label` field.
  * To support multigraphs, equality is based on `ends` and the `label` field.
  * Mix in `GenericHyperEdgeMapper` to get your derived multi-hyperedge also mappable.
  */
abstract class LHyperEdge[+N, L](ends: Several[N])
    extends AbstractHyperEdge[N](ends)
    with OrderedEndpoints
    with SingleLabel[L]
    with ExtendedKeyBySingleLabel
    with LHyperEdgeToString
    with MultiLEdgeToString
