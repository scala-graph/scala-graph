package scalax.collection
package io.dot

import scala.annotation.tailrec
import scala.collection.mutable.{Set => MSet, StringBuilder}

import OuterImplicits._
import edges.DiEdge
import generic.Edge

/** Contains methods to transform `graph` to the DOT language.
  *
  * @define RESP1 a user-supplied function responsible for determining which (sub)graph
  * @define RESP2 should be assigned to and for transforming the passed
  * @define NORMALLY Normally, this method will be called internally but it may also be called for test purposes.
  */
trait Export[N, E <: Edge[N]] extends Any {
  this: Graph2DotExport[N, E] =>

  /** Creates a DOT string by calling the node and edge transformers for the elements of `graph`.
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
  def toDot(
      dotRoot: DotRootGraph,
      edgeTransformer: EdgeTransformer[N, E],
      hEdgeTransformer: Option[HyperEdgeTransformer[N, E]] = None,
      cNodeTransformer: Option[NodeTransformer[N, E]] = None,
      iNodeTransformer: Option[NodeTransformer[N, E]] = None,
      spacing: Spacing = DefaultSpacing
  ): String = {
    val (dotAST: DotAST, root: DotCluster) =
      toAST(dotRoot, edgeTransformer, hEdgeTransformer, cNodeTransformer, iNodeTransformer)
    format(dotRoot, dotAST, root, spacing)
  }

  /** Builds the AST for `graph` employing `dotRoot` and the supplied transformers.
    * $NORMALLY
    */
  def toAST(
      dotRoot: DotRootGraph,
      edgeTransformer: EdgeTransformer[N, E],
      hEdgeTransformer: Option[HyperEdgeTransformer[N, E]] = None,
      cNodeTransformer: Option[NodeTransformer[N, E]] = None,
      iNodeTransformer: Option[NodeTransformer[N, E]] = None
  ): (DotAST, DotCluster) = {
    val root           = DotCluster(dotRoot)
    val dotAST: DotAST = DotAST(root)

    @tailrec def connectClusters(node: dotAST.NodeT): Unit = {
      def conn(ancestor: DotGraph): dotAST.NodeT = {
        val ancestorNode                                                     = dotAST addAndGet DotCluster(ancestor)
        implicit def factory: (DotCluster, DotCluster) => DiEdge[DotCluster] = DiEdge(_, _)
        ancestorNode connectWith node
        ancestorNode
      }
      node.outer.dotGraph match {
        case DotSubGraph(ancestor: DotRootGraph, _, _, _) => conn(ancestor)
        case DotSubGraph(ancestor: DotSubGraph, _, _, _) =>
          conn(ancestor)
          connectClusters(dotAST addAndGet DotCluster(ancestor))
        case _: DotRootGraph =>
      }
    }

    /* First, visit all edges because they have precedence over nodes when deciding
       on which (sub)graph they should be assigned to.
     */
    val visitedCNodes = MSet.empty[graph.NodeT]
    graph.edges foreach { edge =>
      def dotEdge(edge: graph.EdgeT, dotGraph: DotGraph, edgeStmt: DotEdgeStmt): Unit = {
        val clusterNode = dotAST addAndGet DotCluster(dotGraph)
        connectClusters(clusterNode)

        if (clusterNode.dotStmts add edgeStmt)
          cNodeTransformer map { visitor =>
            edge.ends foreach { node =>
              if (visitedCNodes add node)
                visitor(node) foreach { case (dotGraph, nodeStmt) =>
                  val clusterNode = dotAST addAndGet DotCluster(dotGraph)
                  connectClusters(clusterNode)

                  clusterNode.dotStmts += nodeStmt
                }
            }
          }
      }
      if (edge.isHyperEdge && hEdgeTransformer.isDefined)
        hEdgeTransformer.get(edge) foreach { case (dotGraph, edgeStmt) =>
          dotEdge(edge, dotGraph, edgeStmt)
        }
      else
        edgeTransformer(edge) foreach { case (dotGraph, edgeStmt) =>
          dotEdge(edge, dotGraph, edgeStmt)
        }
    }
    visitedCNodes.clear()

    /* Second, process all isolated nodes.
     */
    iNodeTransformer foreach { visitor =>
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
    val res    = new StringBuilder(graph.size * 20)
    val edgeOp = if (dotRoot.directed) "->" else "--"
    var level  = 0

    def indent(ofGraph: Boolean): Unit =
      if (ofGraph)
        for (_ <- 0 until level)
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

        def outStmtList(stmtList: Seq[DotAttrStmt]): Unit =
          stmtList foreach { case DotAttrStmt(t, attrs) =>
            separate(true)
            res append t.toString
            res append s" [${attrs map format mkString ", "}]"
          }

        def outIdList(kvList: Seq[DotAttr]): Unit =
          kvList foreach { attr =>
            separate(true)
            res append format(attr)
          }

        def outAttrList(attrList: Seq[DotAttr]): Unit =
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

        def head = cluster.dotGraph.headToString

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
              res append s"${nodeId.id}"
              attrList
            case DotEdgeStmt(node_1Id, node_2Id, attrList) =>
              res append s"${node_1Id.id} $edgeOp ${node_2Id.id}"
              attrList
          }
          outAttrList(attrList)
        }

      case _ =>
        level -= 1
        separate(true)
        res append '}'
    }
    res.toString
  }
}
