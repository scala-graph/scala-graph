package scalax.collection.io.json
package imp

import net.liftweb.json._

import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import error.JsonGraphError._, descriptor._, descriptor.Defaults._

// ---------------------------- data structures representing the parse result 
protected[json]
sealed abstract class JsonList(jsonValues: List[JValue])
  extends Iterable[JValue]
{
  override def iterator = jsonValues.iterator
}
protected[json]
sealed abstract class ElemList(elemTypeId: String,
                        elems: List[JValue])
  extends JsonList(elems)
{
  def toString(nodeOrEdge: String) =
    "Json" + (if (elemTypeId.isEmpty) "Default" + nodeOrEdge else elemTypeId) + "List" +
    "(" + mkString(",") + ")"
}
protected[json]
case class NodeList protected[imp] (val nodeTypeId: String,
                                       val nodes: List[JValue])
  extends ElemList(nodeTypeId, nodes)
{
  override def toString = toString("Node")
}
protected[json]
case class EdgeList protected[imp] (val edgeTypeId: String,
                                       val edges:      List[JValue])
  extends ElemList(edgeTypeId, edges)
{
  override def toString = toString("Edge")
}
object Parser {
  // ----------------------------------- parsing JSON text to NodeList/EdgeList 
  def parse[N,C <: EdgeCompanionBase[EdgeLike]]
    (json:       String,
     descriptor: Descriptor[N]): List[ElemList] =
  {
    val jsonAST = JsonParser.parse(json)
    for (JField(name, values) <- jsonAST
         if descriptor.sectionIds contains name) yield
    {
      def makeList(elemTypeId: String, arr: List[JValue]) =
        if (descriptor.sectionIds.isNodes(name))
          if (descriptor.nodeDescriptor(elemTypeId).isEmpty)
            throw err(InvalidElemTypeId, elemTypeId)
          else
            NodeList(elemTypeId, arr)
        else
          if (descriptor.edgeDescriptor(elemTypeId).isEmpty)
            throw err(InvalidElemTypeId, elemTypeId)
          else
            EdgeList(elemTypeId, arr)

      var elemTypeId = defaultId
      val elemList = values match {
        case JObject(obj) =>
          if (obj.size != 1) err(ObjectSizeNEQ1)
          obj.head match {
            case JField(elemTypeId, value) =>
              value match {
                case JArray(arr) => makeList(elemTypeId, arr)
                case _ => throw err(NonArray, value.toString, value.getClass.toString)
              }
          }
        case JArray (arr) => makeList(elemTypeId, arr)
        case _ => throw err(NonObjArrValue, values.toString, values.getClass.toString)
      }
      elemList
    }
  }
}
