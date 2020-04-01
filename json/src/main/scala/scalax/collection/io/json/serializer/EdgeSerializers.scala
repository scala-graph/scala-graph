package scalax.collection.io.json
package serializer

import net.liftweb.json._

import scalax.collection.GraphEdge.Bag
import scalax.collection.io.edge._, scalax.collection.io.edge.Types._

import error.JsonGraphError._

// TODO MappingException
/** Lift-JSON `Serializer` to serialize `EdgeParameters` to JSON arrays of the form
  * ["<n1>","<n2>"] and reversely where <n1> and <n2> represent the node-Ids.
  */
class EdgeSerializer extends Serializer[EdgeParameters] {
  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) =>
      json match {
        case JArray(JString(n1) :: JString(n2) :: Nil) =>
          new EdgeParameters(n1, n2)
        case JObject(JField(_, JString(n1)) :: JField(_, JString(n2)) :: Nil) =>
          new EdgeParameters(n1, n2)
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getSimpleName)
      }
  }
  override def serialize(implicit format: Formats) = {
    case EdgeParameters(nodeId_1, nodeId_2) =>
      JArray(JString(nodeId_1) :: JString(nodeId_2) :: Nil)
  }
}

/** Lift-JSON `Serializer` to serialize `WEdgeParameters` to JSON arrays of the form
  * `["<n1>", "<n2>", <weight>]` and reversely where `<n1>` and `<n2>` represent the node-Ids
  * and `<weight>` a JSON number mapping to `Double`.
  */
class WEdgeSerializer extends Serializer[WEdgeParameters] {
  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) =>
      json match {
        case JArray(JString(n1) :: JString(n2) :: JDouble(weight) :: Nil) =>
          new WEdgeParameters(n1, n2, weight)
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getSimpleName)
      }
  }
  override def serialize(implicit format: Formats) = {
    case WEdgeParameters((nId_1, nId_2), weight) =>
      JArray(JString(nId_1) :: JString(nId_2) :: JDouble(weight) :: Nil)
  }
}

/** #define LSERIALIZER one or more lift-json custom `Serializer`s for labels.
  */
abstract class LSerializer[L: Manifest](labelSerializers: Serializer[L]*) {
  object LabelSerialization {
    implicit val labelFormats = Serialization.formats(NoTypeHints) ++ labelSerializers
    def extract(json: JValue) = json.extract[L]
    def decompose(label: L)   = Extraction.decompose(label)
  }
}

/** Lift-JSON `Serializer` to serialize `LEdgeParameters` to JSON arrays of the form
  * `["<n1>", "<n2>", <label>]` and reversely where `<n1>` and `<n2>` represent the
  * node-Ids and `<label>` any JSON type mapping to `L`.
  *
  * @param labelSerializers $LSERIALIZER
  */
class LEdgeSerializer[L: Manifest](labelSerializers: Serializer[L]*)
    extends LSerializer[L](labelSerializers: _*)
    with Serializer[LEdgeParameters[L]] {
  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) =>
      json match {
        case JArray(JString(n1) :: JString(n2) :: jsonLabel :: Nil) =>
          new LEdgeParameters[L](n1, n2, LabelSerialization.extract(jsonLabel))
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getSimpleName)
      }
  }
  override def serialize(implicit format: Formats) = {
    case LEdgeParameters((nId_1, nId_2), label) =>
      JArray(
        JString(nId_1) :: JString(nId_2) ::
          LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil)
  }
}

/** Lift-JSON `Serializer` to serialize `WLEdgeParameters` to JSON arrays of the form
  * `["<n1>", "<n2>", <weight>, <label>]` and reversely where `<n1>` and `<n2>` represent the
  * node-Ids, `<weight>` a JSON number mapping to `Double` and `<label>` any JSON type
  * mapping to `L`.
  *
  * @param labelSerializers $LSERIALIZER
  */
class WLEdgeSerializer[L: Manifest](labelSerializers: Serializer[L]*)
    extends LSerializer[L](labelSerializers: _*)
    with Serializer[WLEdgeParameters[L]] {
  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) =>
      json match {
        case JArray(JString(n1) :: JString(n2) :: JDouble(weight) :: jsonLabel :: Nil) =>
          new WLEdgeParameters[L](n1, n2, weight, LabelSerialization.extract(jsonLabel))
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getSimpleName)
      }
  }
  override def serialize(implicit format: Formats) = {
    case WLEdgeParameters((nId_1, nId_2), weight, label) =>
      JArray(
        JString(nId_1) :: JString(nId_2) :: JDouble(weight) ::
          LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil)
  }
}
trait HyperEdgeChecker {
  final def checkNodeIds(jsonIds: List[JValue], parameters: HyperEdgeNodeIds) {
    jsonIds.size match {
      case size if size < 2                => throw err(InsufficientNodes, jsonIds.toString)
      case size if size != parameters.size => throw err(UnexpectedNodeId, jsonIds.toString)
      case _                               => None
    }
  }
  final protected def prepareNodes(jsonIds: List[JValue]): List[String] = {
    val par = jsonIds collect { case JString(nId) => nId }
    checkNodeIds(jsonIds, par)
    par
  }
}

/** Lift-JSON `Serializer` to serialize `HyperEdgeParameters` to JSON arrays of the form
  * `["<n1>",...,"<nn>"]` and reversely where `<n1>...<nn>` represent the node-Ids.
  */
class HyperEdgeSerializer extends Serializer[HyperEdgeParameters] with HyperEdgeChecker {
  final private val clazz = classOf[HyperEdgeParameters]
  @inline final private def parameters(jsonIds: List[JValue], kind: String): HyperEdgeParameters =
    new HyperEdgeParameters(prepareNodes(jsonIds), kind)

  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) =>
      json match {
        case JArray(JArray(jsonIds) :: JString(kind) :: Nil) =>
          parameters(jsonIds, kind)
        case JArray(JArray(jsonIds) :: Nil) => // backward compatible with 1.9
          parameters(jsonIds, Bag.toString)
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getSimpleName)
      }
  }
  override def serialize(implicit format: Formats) = {
    case HyperEdgeParameters(nodeIds, kind) =>
      JArray(Extraction.decompose(nodeIds) :: JString(kind) :: Nil)
  }
}

/** Lift-JSON `Serializer` to serialize `WHyperEdgeParameters` to JSON arrays of the form
  * `[ ["<n1>",...,"<nn>"], <weight> ]` and reversely where `<n1>...<nn>` represent the
  * node-Ids and `<weight>` is a `JDouble`.
  */
class WHyperEdgeSerializer extends Serializer[WHyperEdgeParameters] with HyperEdgeChecker {
  final private val clazz = classOf[WHyperEdgeParameters]
  @inline final private def parameters(jsonIds: List[JValue], kind: String, weight: Double): WHyperEdgeParameters =
    new WHyperEdgeParameters(prepareNodes(jsonIds), kind, weight)

  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) =>
      json match {
        case JArray(JArray(jsonIds) :: JString(kind) :: JDouble(weight) :: Nil) =>
          parameters(jsonIds, kind, weight)
        case JArray(JArray(jsonIds) :: JDouble(weight) :: Nil) => // backward compatible with 1.9
          parameters(jsonIds, Bag.toString, weight)
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getSimpleName)
      }
  }
  override def serialize(implicit format: Formats) = {
    case WHyperEdgeParameters(nodeIds, kind, weight) =>
      JArray(Extraction.decompose(nodeIds) :: JString(kind) :: JDouble(weight) :: Nil)
  }
}

/** Lift-JSON `Serializer` to serialize `LHyperEdgeParameters` to JSON arrays of the form
  * `[ ["<n1>",...,"<nn>"], <label> ]` and reversely where `<n1>...<nn>` represent the
  * node-Ids and `<label>` is any subtype of `JValue`.
  *
  * @param labelSerializers $LSERIALIZER
  */
class LHyperEdgeSerializer[L: Manifest](labelSerializers: Serializer[L]*)
    extends LSerializer[L](labelSerializers: _*)
    with Serializer[LHyperEdgeParameters[L]]
    with HyperEdgeChecker {
  final private val clazz = classOf[LHyperEdgeParameters[L]]
  @inline final private def parameters(jsonIds: List[JValue],
                                       kind: String,
                                       jsonLabel: JValue): LHyperEdgeParameters[L] =
    new LHyperEdgeParameters[L](prepareNodes(jsonIds), kind, LabelSerialization.extract(jsonLabel))

  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) =>
      json match {
        case JArray(JArray(jsonIds) :: JString(kind) :: jsonLabel :: Nil) =>
          parameters(jsonIds, kind, jsonLabel)
        case JArray(JArray(jsonIds) :: jsonLabel :: Nil) => // backward compatible with 1.9
          parameters(jsonIds, Bag.toString, jsonLabel)
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getSimpleName)
      }
  }
  override def serialize(implicit format: Formats) = {
    case LHyperEdgeParameters(nodeIds, kind, label) =>
      JArray(
        Extraction.decompose(nodeIds) ::
          JString(kind) ::
          LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil)
  }
}

/** Lift-JSON `Serializer` to serialize `WLHyperEdgeParameters` to JSON arrays of the form
  * `[ ["<n1>",...,"<nn>"], <weight>, <label> ]` and reversely where `<n1>...<nn>`
  * represent the node-Ids, `<weight>` is a `JDouble` and `<label>` is any subtype of `JValue`.
  *
  * @param labelSerializers $LSERIALIZER
  */
class WLHyperEdgeSerializer[L: Manifest](labelSerializers: Serializer[L]*)
    extends LSerializer[L](labelSerializers: _*)
    with Serializer[WLHyperEdgeParameters[L]]
    with HyperEdgeChecker {
  final private val clazz = classOf[WLHyperEdgeParameters[L]]
  @inline final private def parameters(jsonIds: List[JValue],
                                       kind: String,
                                       weight: Double,
                                       jsonLabel: JValue): WLHyperEdgeParameters[L] =
    new WLHyperEdgeParameters[L](prepareNodes(jsonIds), kind, weight, LabelSerialization.extract(jsonLabel))

  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) =>
      json match {
        case JArray(JArray(jsonIds) :: JString(kind) :: JDouble(weight) :: jsonLabel :: Nil) =>
          parameters(jsonIds, kind, weight, jsonLabel)
        case JArray(JArray(jsonIds) :: JDouble(weight) :: jsonLabel :: Nil) => // backward compatible with 1.9
          parameters(jsonIds, Bag.toString, weight, jsonLabel)
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getSimpleName)
      }
  }
  override def serialize(implicit format: Formats) = {
    case WLHyperEdgeParameters(nodeIds, kind, weight, label) =>
      JArray(
        Extraction.decompose(nodeIds) :: JString(kind) :: JDouble(weight) ::
          LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil)
  }
}
