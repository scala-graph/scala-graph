package scalax.collection.hyperedges.labeled

import scalax.collection.generic.{AnyDiHyperEdge, LDiHyperEdgeToString, Label}
import scalax.collection.hyperedges.DiHyperEdge

import scala.collection.immutable.Iterable

/** Template for generic directed hyperedges with a single `label` field.
  * Equality is based solely on the `ends` so this trait is not suitable for multigraphs.
  * Mix in `GenericDiHyperEdgeMapper` to get your derived hyperedge also mappable.
  */
abstract class LDiHyperEdge[+N, L] extends AnyDiHyperEdge[N] with Label[L] with LDiHyperEdgeToString {
  protected def labelToString: String = label.toString
}

/** Template for an `implicit class` that defines the infix constructor `+` to pass a label like `Several(1, 2) ~~> One(3) + aLabel`.
  */
abstract class LDiHyperEdgeInfixConstructor[N, L, CC[X] <: AnyDiHyperEdge[X] with Label[L]](
    apply: (Iterable[N], Iterable[N], L) => CC[N]
) {
  def diHyperedge: DiHyperEdge[N]
  def +(label: L): CC[N] = apply(diHyperedge.sources, diHyperedge.targets, label)
}
