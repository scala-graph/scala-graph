package scalax.collection.io.json
package imp

import language.higherKinds

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
  def createStreams[N, E[N] <: EdgeLikeIn[N]]
     (jsonLists:  List[JsonList],
      descriptor: Descriptor[N]/*,
      failures:   Failures*/): (List[NodeInputStream[N]], List[GenEdgeInputStream[N,E]]) =
  {
    /* Nodes must be processed first to be able to retrieve their id's which are
     * referenced by edges; all created nodes are stored in a map prior to passing
     * them to the Graph in order not to loose their id's.
     */
    val idMap = collection.mutable.Map.empty[String,N]
    val nodes: List[NodeInputStream[N]] =
      for (nodeList <- jsonLists collect { case n: NodeList => n };
           NodeList(typeId, jsonNodes) = nodeList; 
           nodeDescriptor = descriptor.nodeDescriptor(typeId).get) yield
      {
        new NodeInputStream[N] {
          val it =
            (for (jsonNode <- nodeList.view) yield {
              val node = nodeDescriptor.extract(jsonNode)
              val nodeId = nodeDescriptor.id(node)
              val nodeInMap = idMap get nodeId
              if (nodeInMap.isDefined) {
                warn(DuplicateNodeId)
                nodeInMap.get
              } else {
                idMap += nodeId -> node
                node
              }
            }).iterator
          def hasNext = it.hasNext
          def next = it.next
        }
      }
            type AEdge[X] = UnDiEdge[N] with Attributes[N] 
    val edges: List[GenEdgeInputStream[N,E]] =
      for (edgeList <- jsonLists collect { case e: EdgeList => e };
           EdgeList(typeId, edges) = edgeList; 
           edgeDescriptor = descriptor.edgeDescriptor(typeId).get) yield 
      {
        def lookupNode(nodeId: String): N = {
          val nodeInMap = idMap get nodeId
          if (nodeInMap.isDefined) nodeInMap.get
          else                     throw err(UnknownNode, nodeId)
        }
        edgeDescriptor match {
          case d: WLEdgeDescriptor[N,WLUnDiEdge,WLEdgeCompanion[WLUnDiEdge],_] with
                  WLEdgeDescriptor[N,E,WLEdgeCompanion[E],_] =>
            type L = d.aLabel.type
            new EdgeInputStream[N,WLUnDiEdge,L,WLEdgeCompanion,WLEdgeAdapter] (d.edgeCompanion)
              with WLEdgeAdapter[N,WLUnDiEdge,L]
            {
              var params: WLEdgeParameters[L] = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge).asInstanceOf[WLEdgeParameters[L]]
                }).iterator
              def nodes = (lookupNode(params.n1),
                           lookupNode(params.n2))
              def weight = params.weight
              def label  = params.label
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: LEdgeDescriptor[N,LUnDiEdge,LEdgeCompanion[LUnDiEdge],_] with
                  LEdgeDescriptor[N,E,LEdgeCompanion[E],_] =>
            type L = d.aLabel.type
            new EdgeInputStream[N,LUnDiEdge,L,LEdgeCompanion,LEdgeAdapter] (d.edgeCompanion)
              with LEdgeAdapter[N,LUnDiEdge,L]
            {
              var params: LEdgeParameters[L] = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge).asInstanceOf[LEdgeParameters[L]]
                }).iterator
              def nodes = (lookupNode(params.n1),
                           lookupNode(params.n2))
              def label = params.label
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: WEdgeDescriptor[N,WUnDiEdge,WEdgeCompanion[WUnDiEdge]] with
                  WEdgeDescriptor[N,E,WEdgeCompanion[E]] => 
            new EdgeInputStream[N,WUnDiEdge,Nothing,WEdgeCompanion,WEdgeAdapter] (d.edgeCompanion)
              with WEdgeAdapter[N,WUnDiEdge,Nothing]
            {
              var params: WEdgeParameters = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge)
                }).iterator
              def nodes = (lookupNode(params.n1),
                           lookupNode(params.n2))
              def weight = params.weight
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: CEdgeDescriptor[N,CEdge,CEdgeCompanion[CEdge],_] with
                  CEdgeDescriptor[N,E,CEdgeCompanion[E],_] => 
            type P = d.sampleAttributes.type
            new EdgeInputStream[N,CEdge,Nothing,CEdgeCompanion,CEdgeAdapter] (d.edgeCompanion)
              with CEdgeAdapter[N,CEdge,Nothing]
            {
              var params: CEdgeParameters[P] = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge).asInstanceOf[CEdgeParameters[P]]
                }).iterator
              def nodes = (lookupNode(params.n1),
                           lookupNode(params.n2))
              def attributes = params.attributes
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: EdgeDescriptor[N,UnDiEdge,EdgeCompanion[UnDiEdge]] with
                  EdgeDescriptor[N,E,EdgeCompanion[E]] => 
            new EdgeInputStream[N,UnDiEdge,Nothing,EdgeCompanion,EdgeAdapter] (d.edgeCompanion)
              with EdgeAdapter[N,UnDiEdge,Nothing]
            {
              var params: EdgeParameters = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge)
                }).iterator
              def nodes = (lookupNode(params.n1),
                           lookupNode(params.n2))
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: WLHyperEdgeDescriptor[N,WLHyperEdge,WLHyperEdgeCompanion[WLHyperEdge],_] with
                  WLHyperEdgeDescriptor[N,E,WLHyperEdgeCompanion[E],_] => 
            type L = d.aLabel.type
            new EdgeInputStream[N,WLHyperEdge,L,WLHyperEdgeCompanion,WLHyperEdgeAdapter
                               ] (d.edgeCompanion)
              with WLHyperEdgeAdapter[N,WLHyperEdge,L]
            {
              var params: WLHyperEdgeParameters[L] = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge).asInstanceOf[WLHyperEdgeParameters[L]]
                }).iterator
              def nodes = params.nodeIds map lookupNode
              def weight = params.weight
              def label  = params.label
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: LHyperEdgeDescriptor[N,LHyperEdge,LHyperEdgeCompanion[LHyperEdge],_] with
                  LHyperEdgeDescriptor[N,E,LHyperEdgeCompanion[E],_] => 
            type L = d.aLabel.type
            new EdgeInputStream[N,LHyperEdge,L,LHyperEdgeCompanion,LHyperEdgeAdapter
                               ] (d.edgeCompanion)
              with LHyperEdgeAdapter[N,LHyperEdge,L]
            {
              var params: LHyperEdgeParameters[L] = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge).asInstanceOf[LHyperEdgeParameters[L]]
                }).iterator
              def nodes = params.nodeIds map lookupNode
              def label  = params.label
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: WHyperEdgeDescriptor[N,WHyperEdge,WHyperEdgeCompanion[WHyperEdge]] with
                  WHyperEdgeDescriptor[N,E,WHyperEdgeCompanion[E]] => 
            new EdgeInputStream[N,WHyperEdge,Nothing,WHyperEdgeCompanion,WHyperEdgeAdapter
                               ] (d.edgeCompanion)
              with WHyperEdgeAdapter[N,WHyperEdge,Nothing]
            {
              var params: WHyperEdgeParameters = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge)
                }).iterator
              def nodes = params.nodeIds map lookupNode
              def weight = params.weight
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: CHyperEdgeDescriptor[N,CHyperEdge,CHyperEdgeCompanion[CHyperEdge],_] with
                  CHyperEdgeDescriptor[N,E,CHyperEdgeCompanion[E],_] => 
            type P = d.sampleAttributes.type
            new EdgeInputStream[N,CHyperEdge,Nothing,CHyperEdgeCompanion,CHyperEdgeAdapter] (d.edgeCompanion)
              with CHyperEdgeAdapter[N,CHyperEdge,Nothing]
            {
              var params: CHyperEdgeParameters[P] = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge).asInstanceOf[CHyperEdgeParameters[P]]
                }).iterator
              def nodes = params.nodeIds map lookupNode
              def attributes = params.attributes
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case d: HyperEdgeDescriptor[N,HyperEdge,HyperEdgeCompanion[HyperEdge]] with
                  HyperEdgeDescriptor[N,E,HyperEdgeCompanion[E]] => 
            new EdgeInputStream[N,HyperEdge,Nothing,HyperEdgeCompanion,HyperEdgeAdapter
                               ] (d.edgeCompanion)
              with HyperEdgeAdapter[N,HyperEdge,Nothing]
            {
              var params: HyperEdgeParameters = _
              val it = 
                (for (jsonEdge <- edgeList.view) yield {
                  params = d.extract(jsonEdge)
                }).iterator
              def nodes = params.nodeIds map lookupNode
              def hasNext = it.hasNext
              def next = { it.next; this }
            }.asInstanceOf[GenEdgeInputStream[N,E]]

          case ud => throw err(UnexpectedDescr, ud.getClass.getName)
        }
      }
      (nodes, edges)
    }
}