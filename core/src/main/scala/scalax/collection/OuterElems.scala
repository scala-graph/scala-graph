package scalax.collection

import scalax.collection.GraphEdge.EdgeLike

/** Represents parameters that are accepted when calling `Graph(...)`.
  * @tparam N  the type of the nodes (vertices)
  * @tparam E  the kind of the edges (links)
  */
trait OuterElems[N, E <: EdgeLike[N]] {

  sealed trait OuterElem

  /** Wraps any type to be accepted when calling `Graph(...)`. */
  sealed case class OuterNode(node: N) extends OuterElem

  /** To be mixed in by edge classes to allow passing them to `Graph(...)`. */
  sealed case class OuterEdge(edge: E) extends OuterElem
}
