package scalax.collection.io.json
package imp

import net.liftweb.json._

import scalax.collection.GraphEdge.{EdgeCompanionBase, EdgeLike}
import error.JsonGraphError._, descriptor._, descriptor.Defaults._

// ---------------------------- data structures representing the parse result
sealed abstract protected[json] class JsonList(jsonValues: Iterable[JValue]) extends Iterable[JValue] {
  override def iterator = jsonValues.iterator
}
sealed abstract protected[json] class ElemList(elemTypeId: String, elems: Iterable[JValue]) extends JsonList(elems) {
  def toString(nodeOrEdge: String) =
    "Json" + (if (elemTypeId.isEmpty) "Default" + nodeOrEdge else elemTypeId) + "List" +
      "(" + mkString(",") + ")"
}
protected[json] case class NodeList protected[imp] (val nodeTypeId: String, val nodes: Iterable[JValue])
    extends ElemList(nodeTypeId, nodes) {
  override def toString = toString("Node")
}
protected[json] case class EdgeList protected[imp] (val edgeTypeId: String, val edges: Iterable[JValue])
    extends ElemList(edgeTypeId, edges) {
  override def toString = toString("Edge")
}
object Parser {
  // ----------------------------------- parsing JSON text to NodeList/EdgeList
  def parse[N, C <: EdgeCompanionBase[EdgeLike]](json: String, descriptor: Descriptor[N]): Iterable[ElemList] =
    parse(JsonParser.parse(json), descriptor)

  def parse[N, C <: EdgeCompanionBase[EdgeLike]](jsonAST: JValue, descriptor: Descriptor[N]): Iterable[ElemList] =
    jsonAST match {
      case JObject(fields) => parse(fields, descriptor)
      case _               => Seq[ElemList]().toIterable
    }

  def parse[N, C <: EdgeCompanionBase[EdgeLike]](jsonAST: List[JField], descriptor: Descriptor[N]): Iterable[ElemList] =
    (for (JField(name, values) <- jsonAST
          if descriptor.sectionIds contains name)
      yield {
        def makeList(elemTypeId: String, arr: Iterable[JValue]): ElemList =
          if (descriptor.sectionIds.isNodes(name))
            if (descriptor.nodeDescriptor(elemTypeId).isEmpty)
              throw err(InvalidElemTypeId, elemTypeId)
            else
              NodeList(elemTypeId, arr)
          else if (descriptor.edgeDescriptor(elemTypeId).isEmpty)
            throw err(InvalidElemTypeId, elemTypeId)
          else
            EdgeList(elemTypeId, arr)

        values match {
          case JObject(objects) =>
            for (obj <- objects) yield {
              obj match {
                case JField(elemTypeId, value) =>
                  value match {
                    case JArray(arr) => makeList(elemTypeId, arr)
                    case _           => throw err(NonArray, value.toString, value.getClass.toString)
                  }
              }
            }
          case JArray(arr) => Iterable(makeList(defaultId, arr))
          case JNothing    => Iterable(makeList(defaultId, Iterable.empty))
          case _           => throw err(NonObjArrValue, values.toString, values.getClass.toString)
        }
      }).flatten
}
