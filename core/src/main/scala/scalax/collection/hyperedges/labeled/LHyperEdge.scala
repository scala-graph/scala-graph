package scalax.collection
package hyperedges.labeled

import scalax.collection.generic.{AbstractHyperEdge, LHyperEdgeToString, Label}
import scalax.collection.hyperedges._

/** Template for generic undirected hyperedges with a single `label` field.
  * Equality is based solely on the `ends` so this trait is not suitable for multigraphs.
  * Mix in `GenericHyperedgeMapper` to get your derived hyperedge also mappable.
  */
abstract class LHyperEdge[+N, L](ends: Several[N])
    extends AbstractHyperEdge[N](ends)
    with Label[L]
    with LHyperEdgeToString {
  protected def labelToString: String = label.toString
}

/** Template for an `implicit class` that defines the infix constructor `+` to pass a label like `1 ~~ 2 ~~ 3 + aLabel`.
  */
abstract class LHyperEdgeInfixConstructor[N, L, CC[X] <: AbstractHyperEdge[X] with Label[L]](
    apply: (Several[N], L) => CC[N]
) {
  def hyperedge: HyperEdge[N]
  def +(label: L): CC[N] = apply(hyperedge.ends, label)
}
