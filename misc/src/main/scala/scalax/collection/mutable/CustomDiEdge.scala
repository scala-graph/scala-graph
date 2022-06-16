package scalax.collection
package mutable

import OuterImplicits._, GraphEdge._

object LabelType {
  type L = Option[List[Int]]
}
import LabelType.L

/** Labeled directed edge with mutable label of a given type L. */
class CustomDiEdge[+N](nodes: Product)(private var _label: L) extends DiEdge[N](nodes) {

  override def label: L    = _label
  def label_=(newLabel: L) = _label = newLabel
}

object CustomDiEdge {

  def apply[N](from: N, to: N, label: L) =
    new CustomDiEdge[N](NodeProduct(from, to))(label)

  def unapply[N](e: CustomDiEdge[N]) = Some(e)

  implicit final class EdgeAssoc[N](n1: N) {
    def ~+>(n2: N)(l: L) = new CustomDiEdge[N]((n1, n2))(l)
  }
}

object :~> {
  def unapply[N](e: CustomDiEdge[N]): Option[(N, (N, L))] =
    if (e eq null) None else Some(e._1, (e._2, e.label))
}
object + {
  def unapply[N](nl: (N, L)): Option[(N, L)] =
    if (nl eq null) None else Some(nl._1, nl._2)
}

private object TestCustomLDiEdge {
  import CustomDiEdge._
  val outer = 1 ~+> 2 (None)
  outer.label = None

  outer match {
    case n1 :~> n2 + label =>
  }

  import scalax.collection.Graph
  val g     = Graph(outer)
  val inner = g.edges.head
  inner.label = Some(List(1, 2))

  inner.edge match {
    case n1 :~> n2 + label =>
  }
}
