package scalax.collection

import scalax.collection.generic.{AbstractDiEdgeImplicits, AbstractEdgeImplicits}

package object edges {

  /* implicit conversion from `node_1 ~ node_2` to `UnDiEdge(node_1, node_2)`.
   */
  implicit final class UnDiEdgeImplicits[N](override val n1: N)
      extends AnyVal
      with AbstractEdgeImplicits[N, UnDiEdge, UnDiEdge.type] {
    protected def companion = UnDiEdge
  }

  /** Pattern shortcut enabling `edge match { case n1 ~ n2 => f(n1, n2) }`. */
  val ~ = UnDiEdge

  /* implicit conversion from `source ~> target` to `DiEdge(source, target)`.
   */
  implicit final class DiEdgeImplicits[N](val source: N)
      extends AnyVal
      with AbstractDiEdgeImplicits[N, DiEdge, DiEdge.type] {
    protected def companion = DiEdge
  }

  /** Pattern shortcut enabling `edge match { case source ~> target => f(source, target) }`. */
  val ~> = DiEdge
}
