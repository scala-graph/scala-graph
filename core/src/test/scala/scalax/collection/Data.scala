package scalax.collection

import scalax.collection.generic._
import scalax.collection.edges._
import scalax.collection.edges.labeled._

abstract class TGraph[N, E <: Edge[N], G[N, E <: Edge[N]] <: Graph[N, E] with GraphLike[N, E, G]](
    val g: G[N, E]
) {
  def node(outer: N): g.NodeT = g get outer
  def n(outer: N): g.NodeT    = node(outer)
  def edge(outer: E): g.EdgeT = g get outer
  def e(outer: E): g.EdgeT    = edge(outer)
}

/** The Graph for Scala representation of graph pictures located in `scala/test/doc`.
  */
object Data {
// WDi-1.jpg without weights
  val elementsOfDi_1 = List(
    1 ~> 2,
    2 ~> 3,
    4 ~> 3,
    3 ~> 5,
    1 ~> 5,
    1 ~> 3
  )

  // WUnDi-1.jpg without weights
  val elementsOfMixed_1 = List[AnyEdge[Int]](
    1 ~ 2,
    2 ~ 3,
    1 ~> 3,
    1 ~ 5,
    3 ~ 5,
    3 ~ 4,
    4 ~> 4,
    4 ~> 5
  )

  // WUnDi-2.jpg without weights
  val elementsOfMixed_2 = List[AnyEdge[Int]](
    1 ~ 2,
    2 ~ 3,
    1 ~> 3,
    1 ~ 3,
    1 ~> 2,
    2 ~ 2
  )

// WDi-1.jpg
  val elementsOfWDi_1 = List(
    1 ~> 2 % 4,
    2 ~> 3 % 40,
    4 ~> 3 % 7,
    3 ~> 5 % 50,
    1 ~> 5 % 40,
    1 ~> 3 % 2
  )

// WUnDi-1.jpg
  val elementsOfWMixed_1 = List[AnyEdge[Int]](
    1 ~ 2  % 4,
    2 ~ 3  % 2,
    1 ~> 3 % 5,
    1 ~ 5  % 3,
    3 ~ 5  % 2,
    3 ~ 4  % 1,
    4 ~> 4 % 1,
    4 ~> 5 % 0
  )

// WUnDi-2.jpg
  val elementsOfWMixed_2 = List[AnyEdge[Int]](
    1 ~ 2  % 4,
    2 ~ 3  % -1,
    1 ~> 3 % 5,
    1 ~ 3  % 4,
    1 ~> 2 % 3,
    2 ~ 2  % 1
  )
}
