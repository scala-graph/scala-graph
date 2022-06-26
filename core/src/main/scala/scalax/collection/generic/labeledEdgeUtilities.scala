package scalax.collection.generic

/** Intermediate infix extractor for the "unlabeled part" of a weighted edge.
  * It is to be combined with `:~` or `:~>` like `case a :~ b % w`.
  */
case object % {
  def unapply[N](nw: (N, Double)): Option[(N, Double)] = Some(nw._1, nw._2)
}

/** Adds a postfix of the format `% weight` to the `toString` representation of the `Edge`.
  * Useful in combination with `toStringWithParenthesis = false`.
  */
trait WeightToStringPostfix { this: Edge[_] =>
  override def labelToString: String = s" ${%} $weight"
}

trait ExtendedKeyByWeight extends ExtendedKey { this: Edge[_] =>
  override def extendKeyBy: Seq[Double] = weight :: Nil
}
