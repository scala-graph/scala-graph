package scalax.collection.generic

/** Intermediate infix extractor for the "unlabeled part" of a weighted edge.
  * It is to be combined with `:~` or `:~>` like `case a :~ b % w`.
  */
case object % {
  def unapply[N](nw: (N, Double)): Option[(N, Double)] = Some(nw._1, nw._2)
}

trait UnapplyLabeledEdge[N, E <: Edge[N], L] {
  def unapply(edge: E): Option[(N, (N, L))] = Some(edge._1 -> (edge._2, label(edge)))
  protected def label(edge: E): L
}

trait UnapplyLabel[N, L] {
  def unapply(rest: (N, L)): Option[(N, L)] = Some(rest)
}

trait ExtendedKeyByWeight extends ExtendedKey { this: Edge[_] =>
  override def extendKeyBy: Seq[Double] = weight :: Nil
}
