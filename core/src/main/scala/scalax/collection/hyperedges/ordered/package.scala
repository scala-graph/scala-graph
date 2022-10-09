package scalax.collection
package hyperedges

import scalax.collection.generic.{AbstractDiHyperEdgeImplicits, AbstractHyperEdgeImplicits}

/* Intentionally repeated hyperedges.package to facilitate implicit value classes.
 */
package object ordered {

  /* implicit conversion from `node_1 ~~ node_2` to `HyperEdge(node_1, node_2)`.
   */
  implicit final class OrderedHyperFromAny[N](override val n1: N)
      extends AnyVal
      with AbstractHyperEdgeImplicits.FromAny[N, HyperEdge, HyperEdge.type] {
    protected def companion = HyperEdge
  }

  /* implicit conversion from `hyperEdge ~~ node_n` to `HyperEdge(node_1, ..., node_n)`.
   */
  implicit final class OrderedHyperFromEdge[N](override val e1: HyperEdge[N])
      extends AnyVal
      with AbstractHyperEdgeImplicits.FromEdge[N, HyperEdge, HyperEdge.type] {
    protected def companion = HyperEdge
  }

  /** Pattern shortcut enabling `hyperedge match { case ~~(ends) => f(ends) }`. */
  val ~~ = HyperEdge

  /* implicit conversion from `sources ~~> targets` to `DiHyperEdge(source, targets)`.
   */
  implicit final class OrderedDiHyperFromOneOrMore[N](override val sources: OneOrMore[N])
      extends AnyVal
      with AbstractDiHyperEdgeImplicits.FromOneOrMore[N, DiHyperEdge, DiHyperEdge.type] {
    protected def companion = DiHyperEdge
  }

  /** Pattern shortcut enabling `diHyperedge match { case sources ~~> targets => f(sources, targets) }`. */
  val ~~> = DiHyperEdge
}
