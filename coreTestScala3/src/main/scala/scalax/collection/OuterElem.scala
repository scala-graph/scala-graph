package scalax.collection

import scalax.collection.generic._

/** Represents parameters that are accepted when calling `Graph(...)`.
  *
  * @tparam N  the type of the nodes (vertices)
  * @tparam E  the kind of the edges (links)
  */
sealed trait OuterElem[+N, +E <: Edge[N]]

/** Wraps any type to be accepted when calling `Graph(...)`. */
sealed case class OuterNode[+N](node: N) extends OuterElem[N, Nothing]

/** Wraps edges to be accepted when calling `Graph(...)`. */
sealed case class OuterEdge[N, E <: Edge[N]](edge: E) extends OuterElem[N, E]
