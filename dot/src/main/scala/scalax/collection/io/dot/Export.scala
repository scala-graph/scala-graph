package scalax.collection
package io.dot

import scala.language.higherKinds
import scala.collection.mutable.{Set => MSet, StringBuilder}

import GraphPredef.EdgeLikeIn
import GraphEdge.DiEdge

/** Contains methods to transform `graph` to the DOT language.
  *
  * @define RESP1 a user-supplied function responsible for determining which (sub)graph
  * @define RESP2 should be assigned to and for transforming the passed
  * @define NORMALLY Normally, this method will be called internally by `toDot`
  *         but it may also be used for test purposes.
  */
class Export[N, E[X] <: EdgeLikeIn[X]](graph: Graph[N, E]) {

  /** Creates a DOT string by calling the node and edge transformers for the elements
    * of `graph`.
    *
    * @param dotRoot attributes of the root DOT graph.
    * @param edgeTransformer $RESP1 the edge $RESP2 inner edge to a `DotEdgeStmt`.
    *        It is called once for each edge of this graph unless `hyperEdgeTransformer` is defined.
    * @param hEdgeTransformer $RESP1 the edge $RESP2 inner edge to a sequence of `DotEdgeStmt`.
    *        If supplied, it is called once for each hyperedge of this graph.
    * @param cNodeTransformer $RESP1 the '''c'''onnected node $RESP2 inner node to a `DotNodeStmt`.
    *        If supplied, it is called once for each connected node.
    * @param iNodeTransformer $RESP1 the '''i'''solated node $RESP2 inner node to a `DotNodeStmt`.
    *        If supplied, it is called once for each isolated node.
    * @param spacing separation and indentation rules to be followed when building
    *        the DOT language representation of graph.
    */
  def toDot(dotRoot: DotRootGraph,
            edgeTransformer: EdgeTransformer[N, E],
            hEdgeTransformer: Option[HyperEdgeTransformer[N, E]] = None,
            cNodeTransformer: Option[NodeTransformer[N, E]] = None,
            iNodeTransformer: Option[NodeTransformer[N, E]] = None,
            spacing: Spacing = DefaultSpacing): String = {
    val (dotAST: DotAST, root: DotCluster) =
      toAST(dotRoot, edgeTransformer, hEdgeTransformer, cNodeTransformer, iNodeTransformer)
    format(dotRoot, dotAST, root, spacing)
  }

  /** Builds the AST for `graph` employing `dotRoot` and the supplied transformers.
    *  $NORMALLY
    */
  def toAST(dotRoot: DotRootGraph,
            edgeTransformer: EdgeTransformer[N, E],
            hEdgeTransformer: Option[HyperEdgeTransformer[N, E]] = None,
            cNodeTransformer: Option[NodeTransformer[N, E]] = None,
            iNodeTransformer: Option[NodeTransformer[N, E]] = None): (DotAST, DotCluster) = {
    val root           = DotCluster(dotRoot)
    val dotAST: DotAST = DotAST(root)
    def connectClusters(node: dotAST.NodeT): Unit = {
      def conn(ancestor: DotGraph): dotAST.NodeT = {
        val ancestorNode      = dotAST addAndGet DotCluster(ancestor)
        implicit def edgeType = DiEdge
        ancestorNode connectWith node
        ancestorNode
      }
      node.value.dotGraph match {
        case DotSubGraph(ancestor: DotRootGraph, _, _, _) => conn(ancestor)
        case DotSubGraph(ancestor: DotSubGraph, _, _, _) =>
          conn(ancestor)
          connectClusters(dotAST addAndGet DotCluster(ancestor))
        case _: DotRootGraph =>
      }
    }

    /* First we visit all edges because they have precedence over nodes when deciding
       on which (sub)graph they should be assigned to.
     */
    val visitedCNodes = MSet.empty[graph.NodeT]
    graph.edges foreach { edge =>
      def dotEdge(edge: graph.EdgeT, dotGraph: DotGraph, edgeStmt: DotEdgeStmt): Unit = {
        val clusterNode = dotAST addAndGet DotCluster(dotGraph)
        connectClusters(clusterNode)

        if (clusterNode.dotStmts add edgeStmt)
          cNodeTransformer map { visitor =>
            edge foreach { node =>
              if (visitedCNodes add node)
                visitor(node) foreach {
                  case (dotGraph, nodeStmt) =>
                    val clusterNode = dotAST addAndGet DotCluster(dotGraph)
                    connectClusters(clusterNode)

                    clusterNode.dotStmts += nodeStmt
                }
            }
          }
      }
      if (edge.edge.isHyperEdge && hEdgeTransformer.isDefined)
        hEdgeTransformer.get(edge) foreach {
          case (dotGraph, edgeStmt) => dotEdge(edge, dotGraph, edgeStmt)
        } else
        edgeTransformer(edge) foreach {
          case (dotGraph, edgeStmt) =>
            dotEdge(edge, dotGraph, edgeStmt)
        }
    }
    visitedCNodes.clear
    /* Second we process all isolated nodes.
     */
    iNodeTransformer map { visitor =>
      graph.nodes foreach { node =>
        if (node.isIsolated)
          visitor(node) map {
            case (dotGraph, nodeStmt) =>
              val clusterNode = dotAST addAndGet DotCluster(dotGraph)
              connectClusters(clusterNode)
              clusterNode.dotStmts += nodeStmt
            case _ =>
          }
      }
    }
    (dotAST, root)
  }

  /** Formats `dotAST` according to `dotRoot` and `spacing`.
    *  $NORMALLY
    */
  def format(dotRoot: DotRootGraph, dotAST: DotAST, root: DotCluster, spacing: Spacing): String = {
    val res    = new StringBuilder(graph.graphSize * 20)
    val edgeOp = if (dotRoot.directed) "->" else "--"
    var level  = 0
    def indent(ofGraph: Boolean): Unit =
      if (ofGraph)
        for (i <- 0 until level)
          res append Indent(spacing.indent)
    def separate(ofGraph: Boolean): Unit = {
      val sep =
        if (ofGraph) spacing.graphAttrSeparator
        else spacing.elemAttrSeparator
      res append sep
      indent(ofGraph)
    }
    (dotAST get root).innerNodeDownUpTraverser foreach {
      case (true, cluster) =>
        def format(kv: DotAttr): String = s"${kv.name} = ${kv.value}"
        def outStmtList(stmtList: Seq[DotAttrStmt]) {
          stmtList foreach {
            case DotAttrStmt(t, attrs) =>
              separate(true)
              res append t.toString
              res append s" [${attrs map format mkString ", "}]"
          }
        }
        def outIdList(kvList: Seq[DotAttr]) {
          kvList foreach { attr =>
            separate(true)
            res append format(attr)
          }
        }
        def outAttrList(attrList: Seq[DotAttr]) {
          if (attrList.nonEmpty) {
            res append " ["
            attrList foreach { attr =>
              res append attr.name
              if (attr.value().nonEmpty) {
                res append s" = ${attr.value}"
              }
              res append ", "
            }
            res delete (res.size - 2, res.size)
            res append ']'
          }
        }
        val head = cluster.dotGraph.headToString
        val (graphStmtList, graphKvList) = cluster.dotGraph match {
          case DotRootGraph(_, _, _, attrStmts, kvList) =>
            indent(true)
            res append head
            (attrStmts, kvList)
          case DotSubGraph(_, _, attrStmts, kvList) =>
            separate(true)
            res append head
            (attrStmts, kvList)
        }
        level += 1
        outStmtList(graphStmtList)
        outIdList(graphKvList)

        cluster.dotStmts foreach { dotStmt =>
          separate(true)
          val attrList = dotStmt match {
            case DotNodeStmt(nodeId, attrList) =>
              res append s"${nodeId()}"
              attrList
            case DotEdgeStmt(node_1Id, node_2Id, attrList) =>
              res append s"${node_1Id()} $edgeOp ${node_2Id()}"
              attrList
          }
          outAttrList(attrList)
        }
      case up =>
        level -= 1
        separate(true)
        res append '}'
    }
    res.toString
  }
}
