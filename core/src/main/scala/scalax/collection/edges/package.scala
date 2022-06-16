package scalax.collection

import scalax.collection.generic.{AbstractDiEdgeImplicits, AbstractEdgeImplicits}

package object edges {

  /* supports implicit conversion from `node_1 ~ node_2` to `UnDiEdge(node_1, node_2)`.
   */
  implicit class UnDiEdgeImplicits[N](override val n1: N)
      extends AnyVal
      with AbstractEdgeImplicits[N, UnDiEdge, UnDiEdge.type] {
    protected def companion = UnDiEdge
  }

  /** $SHORTCUT `edge match { case n1 ~ n2 => f(n1, n2) }`. */
  val ~ = UnDiEdge

  implicit final class DiEdgeImplicits[N](val source: N)
      extends AnyVal
      with AbstractDiEdgeImplicits[N, DiEdge, DiEdge.type] {
    protected def companion = DiEdge
  }

  /** $SHORTCUT `edge match { case source ~> target => f(source, target) }`. */
  val ~> = DiEdge
}
