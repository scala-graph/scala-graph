package scalax.collection

import scala.collection.immutable.Iterable
import scalax.collection.generic.{AbstractDiHyperEdgeImplicits, AbstractHyperEdgeImplicits}

package object hyperedges {

  /* implicit conversion from `node_1 ~~ node_2` to `HyperEdge(node_1, node_2)`.
   */
  implicit final class HyperFromAny[N](override val n1: N)
      extends AnyVal
      with AbstractHyperEdgeImplicits.FromAny[N, HyperEdge, HyperEdge.type] {
    protected def companion = HyperEdge
  }

  /* implicit conversion from `hyperEdge ~~ node_n` to `HyperEdge(node_1, ..., node_n)`.
   */
  implicit final class HyperFromEdge[N](override val e1: HyperEdge[N])
      extends AnyVal
      with AbstractHyperEdgeImplicits.FromEdge[N, HyperEdge, HyperEdge.type] {
    protected def companion = HyperEdge
  }

  /** Pattern shortcut enabling `hyperedge match { case ~~(ends) => f(ends) }`. */
  val ~~ = HyperEdge

  /* implicit conversion from `source ~~> targets` to `DiHyperEdge(source, targets)`.
   */
  implicit final class DiHyperFromAny[N](override val source: N)
      extends AnyVal
      with AbstractDiHyperEdgeImplicits.FromAny[N, DiHyperEdge, DiHyperEdge.type] {
    protected def companion = DiHyperEdge
  }

  /* implicit conversion from `sources ~~> targets` to `DiHyperEdge(sources, targets)`.
   */
  implicit final class DiHyperFromIterable[N](override val sources: Iterable[N])
      extends AnyVal
      with AbstractDiHyperEdgeImplicits.FromIterable[N, DiHyperEdge, DiHyperEdge.type] {
    protected def companion = DiHyperEdge
  }

  /** Pattern shortcut enabling `diHyperedge match { case sources ~~> targets => f(sources, targets) }`. */
  val ~~> = DiHyperEdge

  type Several[+N] = scalax.collection.Several[N]
  val Several = scalax.collection.Several
}
