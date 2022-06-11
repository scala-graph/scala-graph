package scalax.collection

package object hyperedges {

  /** $SHORTCUT `hyperedge match { case ~~(ends) => f(ends) }`. */
  val ~~ = HyperEdge

  /** $SHORTCUT `hyperedge match { case ~~#(ends) => f(ends) }`. */
  val ~~# = OrderedHyperEdge

  /** $SHORTCUT `diHyperedge match { case sources ~~> targets => f(sources, targets) }`. */
  val ~~> = DiHyperEdge
}
