package scalax.collection.hyperedges
package ordered.labeled

import scalax.collection.generic.{AbstractHyperEdge, LHyperEdgeToString, Label, OrderedEndpoints}

/** Template for generic undirected hyperedges with ordered `ends` a single `label` field.
  * Equality is based solely on the `ends` so this trait is not suitable for multigraphs.
  * Ordered means that `ends` has sequence semantic with respect to equality.
  * Mix in `GenericHyperEdgeMapper` to get your derived hyperedge also mappable.
  */
abstract class LHyperEdge[+N, L](ends: Several[N])
    extends AbstractHyperEdge[N](ends)
    with OrderedEndpoints
    with Label[L]
    with LHyperEdgeToString {
  protected def labelToString: String = label.toString
}

/** Template for an `implicit class` that defines the infix constructor `+` to pass a label like `1 ~~ 2 ~~ 3 + aLabel`.
  */
abstract class LHyperEdgeInfixConstructor[N, L, CC[X] <: AbstractHyperEdge[X] with OrderedEndpoints with Label[L]](
    apply: (Several[N], L) => CC[N]
) {
  def hyperedge: HyperEdge[N]
  def +(label: L): CC[N] = apply(hyperedge.ends, label)
}
