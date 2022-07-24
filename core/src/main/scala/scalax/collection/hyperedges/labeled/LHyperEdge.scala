package scalax.collection.hyperedges.labeled

import scala.collection.immutable.Iterable

import scalax.collection.generic.{AnyHyperEdge, LHyperEdgeToString, Label}
import scalax.collection.hyperedges.HyperEdge

/** Template for generic undirected hyperedges with a single `label` field.
  * Equality is based solely on the `ends` so this trait is not suitable for multigraphs.
  * Mix in `GenericHyperedgeMapper` to get your derived hyperedge also mappable.
  */
abstract class LHyperEdge[+N, L] extends AnyHyperEdge[N] with Label[L] with LHyperEdgeToString {
  protected def labelToString: String = label.toString
}

/** Template for an `implicit class` that defines the infix constructor `+` to pass a label like `1 ~~ 2 ~~ 3 + aLabel`.
  */
abstract class LHyperEdgeInfixConstructor[N, L, CC[X] <: AnyHyperEdge[X] with Label[L]](
    apply: (Iterable[N], L) => CC[N]
) {
  def hyperedge: HyperEdge[N]
  def +(label: L): CC[N] = apply(hyperedge.ends, label)
}
