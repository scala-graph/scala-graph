package scalax.collection

import language.higherKinds
import GraphPredef._, GraphEdge._, edge._, edge.Implicits._

abstract class TGraph[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N,E]) {
  def node(outer: N   ) = g get outer
  def edge(outer: E[N]) = g get outer
}
object Data {
  // WDi-1.jpg without weights
  val outerElemsOfDi_1 = Seq(1~>2, 2~>3, 4~>3, 3~>5, 1~>5, 1~>3)
  val elementsOfDi_1 = Seq[InParam[Int,DiEdge]](outerElemsOfDi_1: _*)
  // WDi-1.jpg
  val elementsOfWDi_1 = Seq[InParam[Int,WDiEdge]](
         1~>2 % 4, 2~>3 % 40, 4~>3 % 7, 3~>5 % 50, 1~>5 % 40, 1~>3 % 2)
  // WUnDi-1.jpg without weights
  val elementsOfUnDi_1 = Seq[InParam[Int,UnDiEdge]](
                         1~2, 2~3, 1~>3, 1~5, 3~5, 3~4, 4~>4, 4~>5)
  // WUnDi-1.jpg
  val elementsofWUnDi_1 = Seq[InParam[Int,WUnDiEdge]](
        1~2 % 4, 2~3 % 2, 1~>3 % 5, 1~5  % 3,
        3~5 % 2, 3~4 % 1, 4~>4 % 1, 4~>5 % 0)
  // WUnDi-2.jpg without weights
  val elementsOfUnDi_2 = Seq[InParam[Int,UnDiEdge]](
                         1~2, 2~3, 1~>3, 1~3, 1~>2, 2~2)
  // WUnDi-2.jpg
  val elementsofWUnDi_2 = Seq[InParam[Int,WUnDiEdge]](
         1~2 % 4, 2~3 % -1, 1~>3 % 5, 1~3 % 4, 1~>2 % 3, 2~2 % 1)
      // 0        1         2         3        4         5
}
