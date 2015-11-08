package scalax.collection.io.json
package imp

import language.higherKinds
import scala.collection.mutable.ArrayBuffer

import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._
import scalax.collection.edge._,
       scalax.collection.edge.WBase._,
       scalax.collection.edge.LBase._,
       scalax.collection.edge.WLBase._,
       scalax.collection.edge.CBase._
import scalax.collection.io._,
       scalax.collection.io.edge._

import descriptor._
import error.JsonGraphError._, error.JsonGraphWarning._

object Stream {
  def createOuterElems[N, E[N] <: EdgeLikeIn[N]]
     (jsonLists:  Iterable[JsonList],
      descriptor: Descriptor[N]/*,
      failures:   Failures*/): (Iterable[N], Iterable[E[N]]) = {

    /* Nodes must be processed first to be able to retrieve their id's which are
     * referenced by edges; all created nodes are stored in a map prior to passing
     * them to the Graph in order not to loose their id's.
     */
    val idMap = collection.mutable.Map.empty[String,N]
    val nodes: Iterable[N] = {
      val buf = new ArrayBuffer[N](512)
      for (nodeList <- jsonLists collect { case n: NodeList => n };
           NodeList(typeId, jsonNodes) = nodeList; 
           nodeDescriptor = descriptor.nodeDescriptor(typeId).get)
        for (jsonNode <- nodeList) yield {
          val node = nodeDescriptor.extract(jsonNode)
          val nodeId = nodeDescriptor.id(node)
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
    type AEdge[X] = UnDiEdge[N] with Attributes[N] 
    val edges: Iterable[E[N]] = {
      val buf = new ArrayBuffer[E[N]](16 * nodes.size)
      for (edgeList <- jsonLists collect { case e: EdgeList => e };
           EdgeList(typeId, edges) = edgeList; 
           edgeDescriptor = descriptor.edgeDescriptor(typeId).get) 
      {
        def lookupNode(nodeId: String): N = {
          val nodeInMap = idMap get nodeId
          if (nodeInMap.isDefined) nodeInMap.get
          else throw err(UnknownNode, nodeId)
        }

        edgeDescriptor match {
          case d: WLEdgeDescriptor[N,WLUnDiEdge,WLEdgeCompanion[WLUnDiEdge],_] with
                  WLEdgeDescriptor[N,E,WLEdgeCompanion[E],_] =>

            type L = d.aLabel.type
            for (jsonEdge <- edgeList)
              buf += {
                val params: WLEdgeParameters[L] =
                    d.extract(jsonEdge).asInstanceOf[WLEdgeParameters[L]]
                d.edgeCompanion(lookupNode(params.n1), lookupNode(params.n2))(
                                params.weight, params.label)
              }
            
          case d: LEdgeDescriptor[N,LUnDiEdge,LEdgeCompanion[LUnDiEdge],_] with
                  LEdgeDescriptor[N,E,LEdgeCompanion[E],_] =>

            type L = d.aLabel.type
            for (jsonEdge <- edgeList)
              buf += {
                val params: LEdgeParameters[L] =
                    d.extract(jsonEdge).asInstanceOf[LEdgeParameters[L]]
                d.edgeCompanion(lookupNode(params.n1), lookupNode(params.n2))(
                                params.label)
              }

          case d: WEdgeDescriptor[N,WUnDiEdge,WEdgeCompanion[WUnDiEdge]] with
                  WEdgeDescriptor[N,E,WEdgeCompanion[E]] =>
                    
            for (jsonEdge <- edgeList)
              buf += {
                val params: WEdgeParameters =
                    d.extract(jsonEdge).asInstanceOf[WEdgeParameters]
                d.edgeCompanion(lookupNode(params.n1), lookupNode(params.n2))(
                                params.weight)
              }

          case d: CEdgeDescriptor[N,CEdge,CEdgeCompanion[CEdge],_] with
                  CEdgeDescriptor[N,E,CEdgeCompanion[E],_] =>

            type P = d.sampleAttributes.type
            for (jsonEdge <- edgeList)
              buf += {
                val params: CEdgeParameters[P] =
                    d.extract(jsonEdge).asInstanceOf[CEdgeParameters[P]]
                d.edgeCompanion(
                    lookupNode(params.n1), lookupNode(params.n2),
                    params.attributes.asInstanceOf[d.edgeCompanion.P]).asInstanceOf[E[N]]
              }

          case d: EdgeDescriptor[N,UnDiEdge,EdgeCompanion[UnDiEdge]] with
                  EdgeDescriptor[N,E,EdgeCompanion[E]] =>
                    
            for (jsonEdge <- edgeList)
              buf += {
                val params: EdgeParameters =
                    d.extract(jsonEdge).asInstanceOf[EdgeParameters]
                d.edgeCompanion(lookupNode(params.n1), lookupNode(params.n2))
              }

          case d: WLHyperEdgeDescriptor[N,WLHyperEdge,WLHyperEdgeCompanion[WLHyperEdge],_] with
                  WLHyperEdgeDescriptor[N,E,WLHyperEdgeCompanion[E],_] =>
                    
            type L = d.aLabel.type
            for (jsonEdge <- edgeList)
              buf += {
                val params: WLHyperEdgeParameters[L] =
                    d.extract(jsonEdge).asInstanceOf[WLHyperEdgeParameters[L]]
                d.edgeCompanion(params.nodeIds map lookupNode)(
                                params.weight, params.label)(CollectionKind.from(params.endpointsKind))
              }

          case d: LHyperEdgeDescriptor[N,LHyperEdge,LHyperEdgeCompanion[LHyperEdge],_] with
                  LHyperEdgeDescriptor[N,E,LHyperEdgeCompanion[E],_] =>

            type L = d.aLabel.type
            for (jsonEdge <- edgeList)
              buf += {
                val params: LHyperEdgeParameters[L] =
                    d.extract(jsonEdge).asInstanceOf[LHyperEdgeParameters[L]]
                d.edgeCompanion(params.nodeIds map lookupNode)(params.label)(CollectionKind.from(params.endpointsKind))
              }

          case d: WHyperEdgeDescriptor[N,WHyperEdge,WHyperEdgeCompanion[WHyperEdge]] with
                  WHyperEdgeDescriptor[N,E,WHyperEdgeCompanion[E]] =>
                    
            for (jsonEdge <- edgeList)
              buf += {
                val params: WHyperEdgeParameters =
                    d.extract(jsonEdge).asInstanceOf[WHyperEdgeParameters]
                d.edgeCompanion(params.nodeIds map lookupNode)(params.weight)(CollectionKind.from(params.endpointsKind))                                
              }

          case d: CHyperEdgeDescriptor[N,CHyperEdge,CHyperEdgeCompanion[CHyperEdge],_] with
                  CHyperEdgeDescriptor[N,E,CHyperEdgeCompanion[E],_] =>
                    
            type P = d.sampleAttributes.type
            for (jsonEdge <- edgeList)
              buf += {
                val params: CHyperEdgeParameters[P] =
                    d.extract(jsonEdge).asInstanceOf[CHyperEdgeParameters[P]]
                d.edgeCompanion(params.nodeIds map lookupNode,
                                params.attributes.asInstanceOf[d.edgeCompanion.P])(
                                CollectionKind.from(params.endpointsKind)).asInstanceOf[E[N]]
              }

          case d: HyperEdgeDescriptor[N,HyperEdge,HyperEdgeCompanion[HyperEdge]] with
                  HyperEdgeDescriptor[N,E,HyperEdgeCompanion[E]] =>
                    
            for (jsonEdge <- edgeList)
              buf += {
                val params: HyperEdgeParameters =
                    d.extract(jsonEdge).asInstanceOf[HyperEdgeParameters]
                val nodeIds = params.nodeIds map lookupNode
                var rest = nodeIds
                val _1 = rest.head; rest = rest.tail
                val _2 = rest.head; rest = rest.tail
                val _n = rest
                d.edgeCompanion(_1, _2, rest: _*)
              }

          case ud => throw err(UnexpectedDescr, ud.getClass.getName)
        }
      }
      buf
    }
    (nodes, edges)
  }
}