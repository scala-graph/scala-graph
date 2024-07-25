package scalax.collection.io.json
package exp

import scala.collection.Set
import scala.collection.immutable.Iterable

import net.liftweb.json._

import scalax.collection.generic.Edge
import scalax.collection.AnyGraph

import descriptor._

class Export[N, E <: Edge[N]](
    graph: AnyGraph[N, E],
    descriptor: Descriptor[N]
)(implicit simpleClassNames: Boolean = true) {

  def jsonASTNodes: JField = {
    val classNodesMap =
      graph.nodes.toOuter groupBy { (a: Any) =>
        val clazz = a.asInstanceOf[AnyRef].getClass
        if (simpleClassNames) clazz.getSimpleName
        else clazz.getName
      }

    case class NodeValues(classNodes: (String, Set[N])) {
      val descr = descriptor.nodeDescriptor(classNodes._2.head)
      val jNodes: List[JValue] =
        (for (node <- classNodes._2) yield descr.decompose(node)).toList

      def jField: JField = JField(descr.typeId, JArray(jNodes))

      def jValue: JValue =
        if (descriptor.hasSingleNodeDescriptor) JArray(jNodes)
        else JObject(List(jField))
    }

    JField(
      descriptor.sectionKeys.nodesId,
      classNodesMap.size match {
        case 0 => JNothing
        case 1 => NodeValues(classNodesMap.head).jValue
        case _ =>
          JObject(
            (for (classNodes <- classNodesMap)
              yield NodeValues(classNodes).jField).toList
          )
      }
    )
  }

  def jsonASTEdges: JField = {
    implicit val descriptor: Descriptor[N] = this.descriptor
    val classEdgesMap                      = graph.edges.toOuter groupBy (_.getClass)

    case class EdgeValues(classEdges: (Class[_ <: E], Set[E])) {
      val (descr, jEdges: List[JValue]) = descriptor.edgeDescriptor(classEdges._1) match {
        case d: EdgeDescriptorBase[N @unchecked, E @unchecked] =>
          (d, (for (edge <- classEdges._2) yield d.decompose(edge)).toList)
      }

      def jArray: JArray = JArray(jEdges)
      def jField: JField = JField(descr.typeId, jArray)
      def jValue: JValue =
        if (descriptor.hasSingleEdgeDescriptor) jArray
        else JObject(List(jField))
    }

    JField(
      descriptor.sectionKeys.edgesId,
      classEdgesMap.size match {
        case 0 => JNothing
        case 1 => EdgeValues(classEdgesMap.head).jValue
        case _ =>
          JObject(
            (for (classEdges <- classEdgesMap)
              yield EdgeValues(classEdges).jField).toList
          )
      }
    )
  }

  def jsonAST(parts: Iterable[JField]): JObject = JObject(parts.toList)
  def jsonText(obj: JObject): String            = compactRender(obj)
}
