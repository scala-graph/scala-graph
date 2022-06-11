package scalax.collection

package object edges {

  /** $SHORTCUT `edge match { case n1 ~ n2 => f(n1, n2) }`. */
  val ~ = UnDiEdge

  /** $SHORTCUT `edge match { case source ~> target => f(source, target) }`. */
  val ~> = DiEdge
}
