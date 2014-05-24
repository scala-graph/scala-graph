package scalax.collection
package io.dot

import language.higherKinds
import collection.mutable.{Map => MMap, Set => MSet, StringBuilder}
import util.matching.Regex

import mutable.{Graph => MGraph}
import GraphPredef.EdgeLikeIn
import GraphEdge.DiEdge

/**
 * Contains methods to transform `graph` to the DOT language.
 * 
 * @define RESP1 a user-supplied function responsible for determining which (sub)graph
 * @define RESP2 should be assigned to and for transforming the passed
 * @define NORMALLY Normally, this method will be called internally by `toDot`
 *         but it may also be used for test purposes. 
 */
class Export[N, E[X] <: EdgeLikeIn[X]](graph: Graph[N,E]) {
  /**
   * Creates a DOT string by calling the node and edge transformers for the elements
   * of `graph`.
   *
   * @param dotRoot attributes of the root DOT graph.
   * @param edgeTransformer $RESP1 the edge $RESP2 inner edge to a `DotEdgeStmt`.
   *        It is called once for each edge of this `graph`.
   * @param cNodeTransformer $RESP1 the '''c'''onnected node $RESP2 inner node to a `DotNodeStmt`.
   *        If supplied, it is called once for each connected node.
   * @param iNodeTransformer $RESP1 the '''i'''solated node $RESP2 inner node to a `DotNodeStmt`.
   *        If supplied, it is called once for each isolated node.
   * @param spacing separation and indentation rules to be followed when building
   *        the DOT language representation of `graph`.
   */
  def toDot(dotRoot:          DotRootGraph,
            edgeTransformer:  Graph[N,E]#EdgeT          => Option[(DotGraph, DotEdgeStmt)],
            cNodeTransformer: Option[(Graph[N,E]#NodeT) => Option[(DotGraph, DotNodeStmt)]] = None,
            iNodeTransformer: Option[ Graph[N,E]#NodeT  => Option[(DotGraph, DotNodeStmt)]] = None,
            spacing:          Spacing = DefaultSpacing): String =
  {
    val (dotAST: DotAST, root: DotCluster) =
      toAST(dotRoot, edgeTransformer, cNodeTransformer, iNodeTransformer)
    format(dotRoot, dotAST, root, spacing)
  }
  /** Builds the AST for `graph` employing `dotRoot` and the supplied transformers.
   *  $NORMALLY
   */
  def toAST(dotRoot:          DotRootGraph,
            edgeTransformer:  Graph[N,E]#EdgeT          => Option[(DotGraph, DotEdgeStmt)],
            cNodeTransformer: Option[(Graph[N,E]#NodeT) => Option[(DotGraph, DotNodeStmt)]] = None,
            iNodeTransformer: Option[ Graph[N,E]#NodeT  => Option[(DotGraph, DotNodeStmt)]] = None)
      : (DotAST, DotCluster) =
  {
    val root = DotCluster(dotRoot)
    val dotAST: DotAST = DotAST(root)
    def connectClusters(dotGraph: DotGraph, cluster: dotAST.NodeT) {
      dotGraph match {
        case DotSubGraph(ancestor, _, _) =>
          val ancestorNode = dotAST addAndGet DotCluster(ancestor)
          implicit def edgeType = DiEdge
          ancestorNode connectWith cluster
        case _ =>
      }
    }
    /* First we visit all edges because they have precedence over nodes when deciding
       on which (sub)graph they should be assigned to.
     */
    val visitedCNodes = MSet.empty[graph.NodeT]
    graph.edges foreach { edge =>
      edgeTransformer(edge) foreach { _ match {
        case (dotGraph, edgeStmt) =>
          val cluster = dotAST addAndGet DotCluster(dotGraph)
          connectClusters(dotGraph, cluster)

          if (cluster.dotStmts add edgeStmt)
            cNodeTransformer map { visitor =>
              edge foreach { node =>
                if (visitedCNodes add node)
                  visitor(node) foreach { _ match {
                    case (dotGraph, nodeStmt) =>
                      val cluster = dotAST addAndGet DotCluster(dotGraph)
                      connectClusters(dotGraph, cluster)
                      
                      cluster.dotStmts += nodeStmt 
                  }
                }}
            }
        case _ =>
      }}
    }
    visitedCNodes.clear
    /* Second we process all isolated nodes.
     */
    iNodeTransformer map { visitor =>
      graph.nodes foreach { node =>
        if (node.isIsolated)
          visitor(node) map { _ match {
            case (dotGraph, nodeStmt) =>
              val cluster = dotAST addAndGet DotCluster(dotGraph)
              connectClusters(dotGraph, cluster)

              cluster.dotStmts += nodeStmt
            case _ =>
          }}
      }
    }
    (dotAST, root)
  }
  /** Formats `dotAST` according to `dotRoot` and `spacing`.
   *  $NORMALLY
   */
  def format(dotRoot: DotRootGraph,
             dotAST:  DotAST,
             root:    DotCluster,
             spacing: Spacing): String = {
    val res = new StringBuilder(graph.graphSize * 20)
    val edgeOp = if (dotRoot.directed) "->" else "--"
    var level = 0
    def indent(ofGraph: Boolean) =
      if ((if (ofGraph) spacing.graphAttrSeparator
           else         spacing.elemAttrSeparator ) == AttrSeparator.NewLine)
        for (i <- 0 until level)
          res append Indent(spacing.indent)
    def separate(ofGraph: Boolean) {
      val sep = if (ofGraph) spacing.graphAttrSeparator
                else         spacing.elemAttrSeparator
      res append sep
      if (sep == AttrSeparator.NewLine) indent(ofGraph)
    }
    (dotAST get root).innerNodeDownUpTraverser foreach ( _ match {
      case (down, cluster) =>
        if (down) {
          val regIdString = "[a-zA-Z_\200-\377][0-9a-zA-Z_\200-\377]*".r
          val regIdNumeral = "[-]?(.[0-9]+|[0-9]+(.[0-9]*)?)".r
          def toID(s: String): String = {
            def exactMatch(r: Regex) = r findPrefixMatchOf s exists (_.end == s.length)
            if (""""<""".contains(s.head) && """">""".contains(s.last) ||
                exactMatch(regIdNumeral) ||
                exactMatch(regIdString)) s
            else """"%s"""".format(s)
          }
          def outKvList(kvList: Seq[DotAttr]) {
            kvList foreach { attr =>
              separate(true)
              res append toID(attr.name)
              res append " = "
              res append toID(attr.value)
            }
          }
          def outAttrList(attrList: Seq[DotAttr]) {
            if (attrList.nonEmpty) {
              res append " ["
              attrList foreach { attr =>
                res append toID(attr.name)
                if (attr.value.nonEmpty) {
                  res append " = %s".format(toID(attr.value))
                }
                res append ", "
              }
              res delete (res.size - 2, res.size)
              res append ']'
            }
          }
          val head = cluster.dotGraph.headToString
          val graphKvList = cluster.dotGraph match {
            case DotRootGraph(directed, id, strict, kvList) =>
              indent(true)
              res append head
              kvList
            case DotSubGraph(ancestor, id, kvList) =>
              separate(true)
              res append head
              kvList
          }
          level += 1
          outKvList(graphKvList)
          
          cluster.dotStmts foreach { dotStmt =>
            separate(true)
            val attrList = dotStmt match {
              case DotNodeStmt(nodeId, attrList) =>
                res append "%s ".format(toID(nodeId))
                attrList
              case DotEdgeStmt(node_1Id, node_2Id, attrList) =>
                res append "%s %s %s".format(toID(node_1Id), edgeOp, toID(node_2Id))
                attrList
            }
            outAttrList(attrList)
          }
        } else {
          level -= 1
          separate(true)
          res append '}'
        }
      }
    )
    res.toString
  }
}
