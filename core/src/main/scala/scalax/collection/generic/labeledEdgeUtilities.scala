package scalax.collection
package generic

trait SingleLabel[L] { this: Edge[_] with LEdgeToString =>
  def label: L
  protected def labelToString: String = label.toString
}

object SingleLabel {
  def unapply[L](edge: Edge[_] with SingleLabel[L]): Some[L] = Some(edge.label)
}

trait ExtendedKeyBySingleLabel extends MultiEdge { this: Edge[_] with SingleLabel[_] =>
  def extendKeyBy: OneOrMore[Any] = OneOrMore(label)
}

/** Intermediate infix extractor for the "unlabeled part" of a weighted edge.
  * It is to be combined with `:~` or `:~>` like `case a :~ b % w`.
  */
case object % {
  def unapply[N](nw: (N, Double)): Some[(N, Double)] = Some(nw._1, nw._2)
}

trait UnapplyLabeledEdge[N, E <: Edge[N], L] {
  def unapply(edge: E): Some[(N, (N, L))] = Some(edge._1 -> (edge._2, label(edge)))
  protected def label(edge: E): L
}

trait UnapplyLabel[N, L] {
  def unapply(rest: (N, L)): Some[(N, L)] = Some(rest)
}

trait UnapplyGenericLabeledEdge[E[X] <: AnyEdge[X], L] {
  def unapply[N](edge: E[N] with SingleLabel[L]): Some[(N, (N, L))] = Some(edge._1 -> (edge._2, label(edge)))
  protected def label[N](edge: E[N] with SingleLabel[L]): L         = edge.label
}

trait UnapplyGenericLabeledDiHyperEdge[E[X] <: AbstractDiHyperEdge[X], L] {
  def unapply[N](diHyperedge: E[N] with SingleLabel[L]): Some[(OneOrMore[N], (OneOrMore[N], L))] =
    Some(diHyperedge.sources -> (diHyperedge.targets, label(diHyperedge)))

  protected def label[N](edge: E[N] with SingleLabel[L]): L = edge.label
}

trait UnapplyGenericLabel[L] {
  def unapply[N](rest: (N, L)): Some[(N, L)] = Some(rest)
}

trait UnapplyGenericHyperLabel[L] {
  def unapply[N](rest: (OneOrMore[N], L)): Some[(OneOrMore[N], L)] = Some(rest)
}

trait ExtendedKeyByWeight extends MultiEdge { this: Edge[_] =>
  override def extendKeyBy: OneOrMore[Double] = OneOrMore(weight)
}
