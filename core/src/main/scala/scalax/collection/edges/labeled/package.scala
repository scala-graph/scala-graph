package scalax.collection.edges

package object labeled {

  /** Factory shortcut for weighted edges that can be used like `a ~ b % w`.
    */
  implicit final class WUnDiEdgeFactory[N](val e: UnDiEdge[N]) extends AnyVal {
    def %(weight: Double): WUnDiEdge[N] = WUnDiEdge[N](e.source, e.target, weight)
  }

  /** Factory shortcut for weighted edges that can be used like `a ~ b % w`.
    */
  implicit final class WDiEdgeFactory[N](val e: DiEdge[N]) extends AnyVal {
    def %(weight: Double): WDiEdge[N] = WDiEdge[N](e.source, e.target, weight)
  }
}
