package scalax.collection

import language.higherKinds
import GraphPredef._, GraphEdge._

abstract class TGraph[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N,E]) {
  def node(outer: N   ) = g get outer
  def n   (outer: N   ) = node(outer)
  def edge(outer: E[N]) = g get outer
  def e   (outer: E[N]) = edge(outer)
}

/** The Graph for Scala representation of graph pictures located in `scala/test/doc`.
 */
object Data {
  // WDi-1.jpg without weights
  val outerElemsOfDi_1 = List(1~>2, 2~>3, 4~>3, 3~>5, 1~>5, 1~>3)
  val elementsOfDi_1 = List(outerElemsOfDi_1: _*)

  // WUnDi-2.jpg without weights
  val elementsOfUnDi_2 = List(1~2, 2~3, 1~>3, 1~3, 1~>2, 2~2)
}
