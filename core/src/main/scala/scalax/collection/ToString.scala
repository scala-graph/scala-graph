package scalax.collection

import scalax.collection.generic.Edge

protected trait ToString[N, E <: Edge[N], +CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]] {
  this: GraphLike[N, E, CC] =>
  import ToString._

  protected def className: String = "Graph"

  /** Sorts nodes and edges as long as this `Graph` has at most 100 elements.
    * See also `def render`.
    */
  override def toString: String = if (elementCount <= 100) render(SingleLine) else super.toString

  /** Sorts all nodes of this graph by `ordNode` followed by all edges sorted by `ordEdge`
    * and concatenates their string representation `nodeSeparator` and `edgeSeparator`
    * respectively.
    *
    * @param nodeEdgeSetSeparator to separate the node set from the edge set.
    * @param withInnerPrefix      whether the node set and edge set should be prefixed.
    * @param ordNode              the node ordering defaulting to `defaultNodeOrdering`.
    * @param ordEdge              the edge ordering defaulting to `defaultEdgeOrdering`.
    */
  def render(
      style: Style,
      nodeSeparator: String = GraphBase.defaultSeparator,
      edgeSeparator: String = GraphBase.defaultSeparator,
      nodeEdgeSetSeparator: String = GraphBase.defaultSeparator,
      withInnerPrefix: Boolean = true
  )(implicit ordNode: NodeOrdering = defaultNodeOrdering, ordEdge: EdgeOrdering = defaultEdgeOrdering): String = {
    val sets = {
      val setStyle: SetStyle = (style match {
        case s: StyleWithIndent[_] => s.incremented
        case s                     => s
      }).toSetStyle
      List(
        nodes.render(setStyle, nodeSeparator, withInnerPrefix)(ordNode),
        edges.render(setStyle, edgeSeparator, withInnerPrefix)(ordEdge)
      )
    }
    sets.prefixed(style, className, nodeEdgeSetSeparator)
  }

  protected trait SetToString[A] extends AnySet[A] {

    /** Sorts all nodes according to `ord`, concatenates them using `separator`,
      * and prefixes and parenthesizes the result unless `prefix` is blank.
      */
    def render(style: SetStyle, separator: String = GraphBase.defaultSeparator, prefix: String = className)(implicit
        ord: Ordering[A]
    ): String =
      toList.sorted.prefixed(style, prefix, separator)

    /** Sorts all nodes according to `ord`, concatenates them using `separator`,
      * and prefixes and parenthesizes the result if `withPrefix` is `true`.
      */
    def render(style: SetStyle, separator: String, withPrefix: Boolean)(implicit
        ord: Ordering[A]
    ): String =
      render(style, separator, if (withPrefix) className else "")
  }

  protected trait NodeSetToString extends SetToString[NodeT] {
    final override protected def className: String = "NodeSet"
  }

  protected trait EdgeSetToString extends SetToString[EdgeT] {
    final override protected def className: String = "EdgeSet"
  }
}

object ToString {
  sealed trait Style {
    def toSetStyle: SetStyle = this match {
      case s: SetStyle            => s
      case s: SetsOnSeparateLines => SingleLine
    }
  }
  sealed trait StyleWithIndent[C <: StyleWithIndent[C]] extends Style {
    def indent: Int
    def incremented: C
  }
  sealed trait SetStyle extends Style

  object SingleLine extends SetStyle
  final case class SetsOnSeparateLines(indent: Int = 0) extends StyleWithIndent[SetsOnSeparateLines] {
    def incremented: SetsOnSeparateLines = copy(indent + 2)
  }
  final case class SetElemsOnSeparateLines(indent: Int = 0)
      extends StyleWithIndent[SetElemsOnSeparateLines]
      with SetStyle {
    def incremented: SetElemsOnSeparateLines = copy(indent + 2)
  }

  private val lineSeparator = System.lineSeparator

  implicit private class StringEnrichments(val it: Iterable[_]) extends AnyVal {

    def prefixed(style: Style, prefix: String, separator: String): String = {
      val (prefixOpeningBrace, closingBrace) =
        if (prefix.isBlank) ("", "")
        else (s"$prefix(", ")")
      val (open, sep, close) =
        style match {
          case SingleLine =>
            (prefixOpeningBrace, separator, closingBrace)
          case style: StyleWithIndent[_] =>
            def spaces(count: Int) = " " * count
            val newLine            = s"$lineSeparator${spaces(style.incremented.indent)}"
            (
              s"$prefixOpeningBrace${if (it.isEmpty) "" else if (prefix.isBlank) "  " else newLine}",
              s"${separator.trim}$newLine",
              closingBrace
            )
        }
      it.mkString(open, sep, close)
    }
  }
}
