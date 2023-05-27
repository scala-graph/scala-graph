package scalax.collection.io.json
package imp

import scala.collection.mutable.ArrayBuffer

import scalax.collection.generic._
import scalax.collection.io.edge._

import descriptor._
import error.JsonGraphError._, error.JsonGraphWarning._

object Stream {
  def createOuterElems[N, E <: Edge[N]](
      jsonLists: Iterable[JsonList],
      descriptor: Descriptor[N] /*,
      failures:   Failures*/
  ): (Iterable[N], Iterable[E]) = {

    /* Nodes must be processed first to be able to retrieve their id's which are
     * referenced by edges; all created nodes are stored in a map prior to passing
     * them to the Graph in order not to loose their id's.
     */
    val idMap = collection.mutable.Map.empty[String, N]
    val nodes: Iterable[N] = {
      val buf = new ArrayBuffer[N](512)
      for {
        nodeList <- jsonLists collect { case n: NodeList => n }
        NodeList(typeId, jsonNodes) = nodeList
        nodeDescriptor              = descriptor.nodeDescriptor(typeId).get
      } for (jsonNode <- nodeList) yield {
        val node      = nodeDescriptor.extract(jsonNode)
        val nodeId    = nodeDescriptor.id(node)
        val nodeInMap = idMap get nodeId
        buf += (
          if (nodeInMap.isDefined) {
            warn(DuplicateNodeId)
            nodeInMap.get
          } else {
            idMap += nodeId -> node
            node
          }
        )
      }
      buf
    }

    val edges: Iterable[E] = {
      val buf = new ArrayBuffer[E](16 * nodes.size)
      for {
        edgeList <- jsonLists collect { case e: EdgeList => e }
        EdgeList(typeId, edges) = edgeList
        edgeDescriptor          = descriptor.edgeDescriptor(typeId).get
      } {
        def lookupNode(nodeId: String): N = {
          val nodeInMap = idMap get nodeId
          if (nodeInMap.isDefined) nodeInMap.get
          else throw err(UnknownNode, nodeId)
        }

        edgeDescriptor match {
          case d: EdgeDescriptor[N, _] =>
            for (jsonEdge <- edgeList)
              buf += {
                val params = d.extract(jsonEdge)
                d.edgeFactory(lookupNode(params.n1), lookupNode(params.n2)).asInstanceOf[E]
              }
          case d: LEdgeDescriptor[N, _, _] =>
            type L = d.aLabel.type
            for (jsonEdge <- edgeList)
              buf += {
                val params = d.extract(jsonEdge).asInstanceOf[LEdgeParameters[L]]
                d.edgeFactory(lookupNode(params.n1), lookupNode(params.n2), params.label).asInstanceOf[E]
              }
          case d: HyperEdgeDescriptor[N, _] =>
            for (jsonEdge <- edgeList)
              buf += {
                val params: HyperEdgeParameters = d.extract(jsonEdge)
                d.hyperEdgeFactory(params.nodeIds map lookupNode).asInstanceOf[E]
              }
          case d: LHyperEdgeDescriptor[N, _, _] =>
            type L = d.aLabel.type
            for (jsonEdge <- edgeList)
              buf += {
                val params = d.extract(jsonEdge).asInstanceOf[LHyperEdgeParameters[L]]
                d.hyperEdgeFactory(params.nodeIds map lookupNode, params.label).asInstanceOf[E]
              }
          case d: DiHyperEdgeDescriptor[N, _] =>
            for (jsonEdge <- edgeList)
              buf += {
                val params = d.extract(jsonEdge)
                d.diHyperEdgeFactory(params.sources map lookupNode, params.targets map lookupNode).asInstanceOf[E]
              }
          case d: LDiHyperEdgeDescriptor[N, _, _] =>
            type L = d.aLabel.type
            for (jsonEdge <- edgeList)
              buf += {
                val params = d.extract(jsonEdge).asInstanceOf[LDiHyperEdgeParameters[L]]
                d.diHyperEdgeFactory(params.sources map lookupNode, params.targets map lookupNode, params.label)
                  .asInstanceOf[E]
              }
          case ud => throw err(UnexpectedDescr, ud.getClass.getName)
        }
      }
      buf
    }
    (nodes, edges)
  }
}
