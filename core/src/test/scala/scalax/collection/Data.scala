package scalax.collection

import language.higherKinds

import GraphPredef.{EdgeLikeIn, _}
import edge.Implicits._

abstract class TGraph[N, E[X] <: EdgeLikeIn[X], G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]](
    val g: G[N, E]) {
  def node(outer: N): g.NodeT    = g get outer
  def n(outer: N): g.NodeT       = node(outer)
  def edge(outer: E[N]): g.EdgeT = g get outer
  def e(outer: E[N]): g.EdgeT    = edge(outer)
}

/** The Graph for Scala representation of graph pictures located in `scala/test/doc`.
  */
object Data {
  // WDi-1.jpg without weights
  val outerElemsOfDi_1 = List(1 ~> 2, 2 ~> 3, 4 ~> 3, 3 ~> 5, 1 ~> 5, 1 ~> 3)
  val elementsOfDi_1   = List(outerElemsOfDi_1: _*)
  // WDi-1.jpg
  val elementsOfWDi_1 = List(1 ~> 2 % 4, 2 ~> 3 % 40, 4 ~> 3 % 7, 3 ~> 5 % 50, 1 ~> 5 % 40, 1 ~> 3 % 2)

  // WUnDi-1.jpg without weights
  val elementsOfUnDi_1 = List(1 ~ 2, 2 ~ 3, 1 ~> 3, 1 ~ 5, 3 ~ 5, 3 ~ 4, 4 ~> 4, 4 ~> 5)

  // WUnDi-1.jpg
  val elementsOfWUnDi_1 =
    List(1 ~ 2 % 4, 2 ~ 3 % 2, 1 ~> 3 % 5, 1 ~ 5 % 3, 3 ~ 5 % 2, 3 ~ 4 % 1, 4 ~> 4 % 1, 4 ~> 5 % 0)

  // WUnDi-2.jpg without weights
  val elementsOfUnDi_2 = List(1 ~ 2, 2 ~ 3, 1 ~> 3, 1 ~ 3, 1 ~> 2, 2 ~ 2)

  // WUnDi-2.jpg
  val elementsOfWUnDi_2 = List(
    1 ~ 2  % 4,
    2 ~ 3  % -1,
    1 ~> 3 % 5,
    1 ~ 3  % 4,
    1 ~> 2 % 3,
    2 ~ 2  % 1
  )
}
