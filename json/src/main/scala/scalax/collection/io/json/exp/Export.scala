package scalax.collection.io.json
package exp

import scala.language.higherKinds
import scala.collection.Set
import scala.collection.immutable.Iterable

import net.liftweb.json._

import scalax.collection.GraphPredef._, scalax.collection.Graph

import descriptor._

class Export[N, E[X] <: EdgeLikeIn[X]](graph: Graph[N, E], descriptor: Descriptor[N])(
    implicit simpleClassNames: Boolean = true) {
  def jsonASTNodes: JField = {
    def className(a: Any) = {
      val clazz = a.asInstanceOf[AnyRef].getClass
      if (simpleClassNames) clazz.getSimpleName
      else clazz.getName
    }
    val classNodesMap = (for (n <- graph.nodes) yield n.value) groupBy className
    case class NodeValues(classNodes: (String, Set[N])) {
      val descr = descriptor.nodeDescriptor(classNodes._2.head)
      val jNodes: List[JValue] =
        (for (node <- classNodes._2) yield descr.decompose(node)).toList

      def jField: JField = JField(descr.typeId, JArray(jNodes))

      def jValue: JValue =
        if (descr eq descriptor.defaultNodeDescriptor)
          JArray(jNodes)
        else
          JObject(List(jField))
    }
    JField(
      descriptor.sectionIds.nodesId,
      classNodesMap.size match {
        case 0 => JNothing
        case 1 => NodeValues(classNodesMap.head).jValue
        case _ =>
          JObject(
            (for (classNodes <- classNodesMap)
              yield NodeValues(classNodes).jField).toList)
      }
    )
  }
  def jsonASTEdges: JField = {
    implicit val descriptor = this.descriptor
    val classEdgesMap       = (for (e <- graph.edges) yield e.toOuter) groupBy (_.getClass)
    case class EdgeValues(classEdges: (Class[_ <: E[N]], Set[E[N]])) {
      val (descr, jEdges: List[JValue]) = descriptor.edgeDescriptor(classEdges._1) match {
        case d: EdgeDescriptorBase[N, E, _] =>
          (d, (for (edge <- classEdges._2) yield d.decompose(edge)).toList)
      }

      def jArray: JArray = JArray(jEdges)

      def jField: JField = JField(descr.typeId, jArray)

      def jValue: JValue =
        if (descr eq descriptor.defaultEdgeDescriptor) jArray
        else JObject(List(jField))
    }
    JField(
      descriptor.sectionIds.edgesId,
      classEdgesMap.size match {
        case 0 => JNothing
        case 1 => EdgeValues(classEdgesMap.head).jValue
        case _ =>
          JObject(
            (for (classEdges <- classEdgesMap)
              yield EdgeValues(classEdges).jField).toList)
      }
    )
  }
  def jsonAST(parts: Iterable[JField]): JObject = JObject(parts.toList)
  def jsonText(obj: JObject): String            = compactRender(obj)
}
