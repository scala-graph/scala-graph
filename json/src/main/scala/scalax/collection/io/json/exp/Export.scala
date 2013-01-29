package scalax.collection.io.json
package exp

import net.liftweb.json._

import collection.immutable.Iterable

import error.JsonGraphError._, error.JsonGraphWarning._

import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge.EdgeLike,
       scalax.collection.Graph

import descriptor._

class Export[N, E[X] <: EdgeLikeIn[X]] (graph:      Graph[N,E],
                                        descriptor: Descriptor[N])(
                                        implicit simpleClassNames: Boolean = true) {
  def jsonASTNodes: Iterable[JField] = {
    def className(a: Any) = {
      val clazz = a.asInstanceOf[AnyRef].getClass
      if (simpleClassNames) clazz.getSimpleName
      else                  clazz.getName
    }
    val classNodesMap = (for (n <- graph.nodes) yield n.value) groupBy (className(_))
    for (classNodes <- classNodesMap;
         val descr = descriptor.nodeDescriptor(classNodes._2.head)) yield {
      val jNodes = JArray(
          (for (node <- classNodes._2) yield descr.decompose(node)) toList)
      JField(
        descriptor.sectionIds.nodesId,
        if (descr eq descriptor.defaultNodeDescriptor) jNodes
        else JObject(List(JField(descr.typeId, jNodes)))
      )
    }
  }
  def jsonASTEdges: Iterable[JField] = {
    implicit val descriptor = this.descriptor
    val classEdgesMap = (for (e <- graph.edges) yield e.toEdgeIn) groupBy (_.getClass)
    for (classEdges <- classEdgesMap) yield
      descriptor.edgeDescriptor(classEdges._1) match {
        case d: EdgeDescriptorBase[N,E,_] =>
          val jEdges = JArray(
              (for (edge <- classEdges._2) yield d.decompose(edge)) toList)
        JField(
          descriptor.sectionIds.edgesId,
          if (d eq descriptor.defaultEdgeDescriptor)
            jEdges
          else
            JObject(List(JField(d.typeId, jEdges)))
        )
      }
  }
  def jsonAST(parts: Iterable[JField]) = JObject(parts toList)
  def jsonText(obj: JObject) = compact(render(obj))
}