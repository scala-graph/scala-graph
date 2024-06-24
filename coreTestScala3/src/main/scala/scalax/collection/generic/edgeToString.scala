package scalax.collection.generic

sealed protected trait EdgeToString { e: Edge[_] =>

  protected def nodesToStringSeparator: String

  protected def nodesToString: String = ends.iterator mkString nodesToStringSeparator

  /** Implementation in term of the protected method `nodesToString`. */
  override def toString: String = nodesToString
}

trait UnDiEdgeToString extends EdgeToString { e: AnyUnDiEdge[_] =>
  override protected def nodesToStringSeparator: String = " ~ "
}

trait DiEdgeToString extends EdgeToString { e: AnyDiEdge[_] =>
  override protected def nodesToStringSeparator: String = " ~> "
}

trait HyperEdgeToString extends EdgeToString { e: AnyHyperEdge[_] =>
  override protected def nodesToStringSeparator: String = " ~~ "
}

trait DiHyperEdgeToString extends EdgeToString { e: AnyDiHyperEdge[_] =>

  override protected def nodesToStringSeparator: String = " ~~> "

  override protected def nodesToString: String = {
    def part(nodes: IterableOnce[_]): String = s"""{${nodes.iterator mkString ", "}}"""
    s"${part(sources.iterator)}$nodesToStringSeparator${part(targets.iterator)}"
  }
}

sealed protected trait LEdgeToString extends EdgeToString { e: Edge[_] =>

  /** The part to be printed after the edge ends and before the label. */
  protected def labelSeparator: String = " :+ "

  /** The part to be printed after the edge ends and the label separator. */
  protected def labelToString: String

  /** Implemented in terms of the protected methods `nodesToString`, `labelSeparator` and  `labelToString`. */
  override def toString: String = s"$nodesToString$labelSeparator$labelToString"
}

trait LUnDiEdgeToString extends LEdgeToString with UnDiEdgeToString { this: AnyUnDiEdge[_] => }

trait LDiEdgeToString extends LEdgeToString with DiEdgeToString { this: AnyDiEdge[_] => }

trait LHyperEdgeToString extends LEdgeToString with HyperEdgeToString { this: AbstractHyperEdge[_] => }

trait LDiHyperEdgeToString extends LEdgeToString with DiHyperEdgeToString { this: AbstractDiHyperEdge[_] => }

/** Mix in this trait in your labeled edge class if you want to indicate in its `toString` representation
  * that the edge supports multigraphs. Once mixed in, the `labelSeparator` gets `:++`.
  */
trait MultiLEdgeToString { e: LEdgeToString with MultiEdge =>

  override protected def labelSeparator: String = " :++ "
}

sealed protected trait WEdgeToString extends LEdgeToString { e: Edge[_] =>

  override protected def labelSeparator: String = " % "

  /** The part to be printed after the edge ends and the label separator. */
  protected def labelToString: String = weight.toString
}

trait WUnDiEdgeToString extends WEdgeToString with UnDiEdgeToString { e: AnyUnDiEdge[_] => }

trait WDiEdgeToString extends WEdgeToString with DiEdgeToString { e: AnyDiEdge[_] => }

trait WHyperEdgeToString extends WEdgeToString with HyperEdgeToString { e: AnyHyperEdge[_] => }

trait WDiHyperEdgeToString extends WEdgeToString with DiHyperEdgeToString { e: AnyDiHyperEdge[_] => }

trait MultiWEdgeToString { e: WEdgeToString with MultiEdge =>

  override protected def labelSeparator: String = " %% "
}
