package scalax.collection

import scalax.collection.generic.Edge

protected trait ToString[N, E <: Edge[N], +CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]] {
  this: GraphLike[N, E, CC] =>
  protected def className: String = "Graph"

  /** Sorts nodes and edges as long as this `Graph` has at most 100 elements.
    * See also `asSortedString` and `toSortedString`.
    */
  override def toString: String = if (elementCount <= 100) toSortedString()() else super.toString

  /** Sorts all nodes of this graph by `ordNode` followed by all edges sorted by `ordEdge`
    * and concatenates their string representation `nodeSeparator` and `edgeSeparator`
    * respectively.
    *
    * @param nodeSeparator        to separate nodes by.
    * @param edgeSeparator        to separate edges by.
    * @param nodesEdgesSeparator  to separate nodes from edges by.
    * @param withNodesEdgesPrefix whether the node and edge set should be prefixed.
    * @param ordNode              the node ordering defaulting to `defaultNodeOrdering`.
    * @param ordEdge              the edge ordering defaulting to `defaultEdgeOrdering`.
    */
  def asSortedString(
      nodeSeparator: String = GraphBase.defaultSeparator,
      edgeSeparator: String = GraphBase.defaultSeparator,
      nodesEdgesSeparator: String = GraphBase.defaultSeparator,
      withNodesEdgesPrefix: Boolean = false
  )(implicit ordNode: NodeOrdering = defaultNodeOrdering, ordEdge: EdgeOrdering = defaultEdgeOrdering): String = {
    val ns =
      if (withNodesEdgesPrefix) nodes.toSortedString(nodeSeparator)(ordNode)
      else nodes.asSortedString(nodeSeparator)(ordNode)
    val es =
      if (withNodesEdgesPrefix) edges.toSortedString(edgeSeparator)(ordEdge)
      else edges.asSortedString(edgeSeparator)(ordEdge)
    ns + (if (ns.nonEmpty && es.nonEmpty) nodesEdgesSeparator else "") +
    es
  }

  /** Same as `asSortedString` but additionally prefixed and parenthesized by `stringPrefix`.
    */
  def toSortedString(
      nodeSeparator: String = GraphBase.defaultSeparator,
      edgeSeparator: String = GraphBase.defaultSeparator,
      nodesEdgesSeparator: String = GraphBase.defaultSeparator,
      withNodesEdgesPrefix: Boolean = false
  )(implicit
      ordNode: NodeOrdering = defaultNodeOrdering,
      ordEdge: EdgeOrdering = defaultEdgeOrdering
  ): String =
    className +
      "(" + asSortedString(nodeSeparator, edgeSeparator, nodesEdgesSeparator, withNodesEdgesPrefix)(ordNode, ordEdge) +
      ")"

  protected trait ToStringNodeSet extends AnySet[NodeT] {
    final override protected def className: String = "NodeSet"

    /** Sorts all nodes according to `ord` and concatenates them using `separator`.
      *
      * @param separator to separate nodes by.
      * @param ord       custom ordering.
      * @return sorted and concatenated string representation of this node set.
      */
    def asSortedString(separator: String = GraphBase.defaultSeparator)(implicit
        ord: NodeOrdering = defaultNodeOrdering
    ): String =
      toList.sorted(ord) mkString separator

    /** Sorts all nodes according to `ord`, concatenates them using `separator`
      * and prefixes and parenthesizes the result with `stringPrefix`.
      *
      * @param separator to separate nodes by.
      * @param ord       custom ordering.
      * @return sorted, concatenated and prefixed string representation of this node set.
      */
    def toSortedString(separator: String = GraphBase.defaultSeparator)(implicit
        ord: NodeOrdering = defaultNodeOrdering
    ): String =
      stringPrefix + "(" + asSortedString(separator)(ord) + ")"
  }

  protected trait ToStringEdgeSet extends AnySet[EdgeT] {
    final override protected def className: String = "EdgeSet"

    /** Sorts all edges according to `ord` and concatenates them using `separator`.
      *
      * @param separator to separate edges by.
      * @param ord       custom ordering.
      * @return sorted and concatenated string representation of this edge set.
      */
    def asSortedString(separator: String = GraphBase.defaultSeparator)(implicit
        ord: EdgeOrdering = defaultEdgeOrdering
    ) =
      toList.sorted(ord) mkString separator

    /** Sorts all edges according to `ord`, concatenates them using `separator`
      * and prefixes and parenthesizes the result with `stringPrefix`.
      *
      * @param separator to separate edges by.
      * @param ord       custom ordering.
      * @return sorted, concatenated and prefixed string representation of this edge set.
      */
    def toSortedString(separator: String = GraphBase.defaultSeparator)(implicit
        ord: EdgeOrdering = defaultEdgeOrdering
    ): String =
      stringPrefix + "(" + asSortedString(separator)(ord) + ")"
  }
}
