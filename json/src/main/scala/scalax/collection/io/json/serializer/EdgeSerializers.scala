package scalax.collection.io.json
package serializer

import net.liftweb.json._

import scalax.collection.io.edge._,
       scalax.collection.io.edge.Types._

import error.JsonGraphIssue, error.JsonGraphError._, error.JsonGraphWarning._

// TODO MappingException
/** Lift-JSON `Serializer` to serialize `EdgeParameters` to JSON arrays of the form
 * ["<n1>","<n2>"] and reversely where <n1> and <n2> represent the node-Ids. 
 */
class EdgeSerializer extends Serializer[EdgeParameters] { 
  private val clazz = classOf[EdgeParameters] 
  override def deserialize(implicit format: Formats) = { 
    case (TypeInfo(clazz, _), json) => json match { 
      case JArray(JString(n1) :: JString(n2) :: Nil) =>
           new EdgeParameters(n1, n2) 
      case x => throw new MappingException(
                "Can't convert " + x + " to " + clazz.getSimpleName) 
    } 
  } 
  override def serialize(implicit format: Formats) = { 
    case EdgeParameters(nodeId_1,nodeId_2) =>
      JArray(JString(nodeId_1) :: JString(nodeId_2) :: Nil) 
  } 
} 
/** Lift-JSON `Serializer` to serialize `WEdgeParameters` to JSON arrays of the form
 * `["<n1>", "<n2>", <weight>]` and reversely where `<n1>` and `<n2>` represent the node-Ids
 * and `<weight>` a JSON number mapping to `Long`. 
 */
class WEdgeSerializer extends Serializer[WEdgeParameters] { 
  private val clazz = classOf[WEdgeParameters] 
  override def deserialize(implicit format: Formats) = { 
    case (TypeInfo(clazz, _), json) => json match { 
      case JArray(JString(n1) :: JString(n2) :: JInt(weight) :: Nil) =>
           new WEdgeParameters(n1, n2, weight.toLong) 
      case x => throw new MappingException(
                "Can't convert " + x + " to " + clazz.getSimpleName) 
    } 
  } 
  override def serialize(implicit format: Formats) = { 
    case WEdgeParameters((nId_1,nId_2), weight) =>
      JArray(JString(nId_1) :: JString(nId_2) :: JInt(weight) :: Nil) 
  } 
}
/**
 * #define LSERIALIZER one or more lift-json custom `Serializer`s for labels.
 */
abstract class LSerializer[L: Manifest] (labelSerializers: Serializer[L]*) {
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
class LEdgeSerializer[L: Manifest] (labelSerializers: Serializer[L]*)
  extends LSerializer[L](labelSerializers: _*)
  with    Serializer [LEdgeParameters[L]]
{ 
  private val clazz = classOf[LEdgeParameters[_]] 
  override def deserialize(implicit format: Formats) = { 
    case (TypeInfo(clazz, _), json) => json match { 
      case JArray(JString(n1) :: JString(n2) :: jsonLabel :: Nil) =>
           new LEdgeParameters[L](n1, n2, LabelSerialization.extract(jsonLabel))
      case x => throw new MappingException(
                "Can't convert " + x + " to " + clazz.getSimpleName) 
    } 
  } 
  override def serialize(implicit format: Formats) = { 
    case LEdgeParameters((nId_1,nId_2), label) =>
      JArray(JString(nId_1) :: JString(nId_2) ::
             LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil) 
  } 
}
/** Lift-JSON `Serializer` to serialize `WLEdgeParameters` to JSON arrays of the form
 * `["<n1>", "<n2>", <weight>, <label>]` and reversely where `<n1>` and `<n2>` represent the
 * node-Ids, `<weight>` a JSON number mapping to `Long` and `<label>` any JSON type
 * mapping to `L`.
 * 
 * @param labelSerializers $LSERIALIZER
 */
class WLEdgeSerializer[L: Manifest] (labelSerializers: Serializer[L]*)
  extends LSerializer[L](labelSerializers: _*)
  with    Serializer [WLEdgeParameters[L]]
{ 
  private val clazz = classOf[WLEdgeParameters[_]] 
  override def deserialize(implicit format: Formats) = { 
    case (TypeInfo(clazz, _), json) => json match { 
      case JArray(JString(n1) :: JString(n2) :: JInt(weight) :: jsonLabel :: Nil) =>
           new WLEdgeParameters[L](n1, n2, weight.toLong,
                                   LabelSerialization.extract(jsonLabel))
      case x => throw new MappingException(
                "Can't convert " + x + " to " + clazz.getSimpleName) 
    } 
  } 
  override def serialize(implicit format: Formats) = { 
    case WLEdgeParameters((nId_1,nId_2), weight, label) =>
      JArray(JString(nId_1) :: JString(nId_2) :: JInt(weight) ::
             LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil) 
  } 
}
trait HyperEdgeChecker {
  def checkNodeIds (jsonIds: List[JValue],
                    parameters: HyperEdgeNodeIds) {
    jsonIds.size match {
      case size if size < 2
           => throw err(InsufficientNodes, jsonIds.toString)
      case size if size != parameters.size
           => throw err(UnexpectedNodeId,  jsonIds.toString)
      case _ => None
    }
  }
}
/** Lift-JSON `Serializer` to serialize `HyperEdgeParameters` to JSON arrays of the form
 * `["<n1>",...,"<nn>"]` and reversely where `<n1>...<nn>` represent the node-Ids. 
 */
class HyperEdgeSerializer
  extends Serializer[HyperEdgeParameters]
  with    HyperEdgeChecker
{ 
  private val clazz = classOf[HyperEdgeParameters] 
  override def deserialize(implicit format: Formats) = { 
    case (TypeInfo(clazz, _), json) => json match { 
      case JArray(jsonIds) =>
        val par = jsonIds collect {case JString(nId) => nId}
        checkNodeIds(jsonIds, par)
        new HyperEdgeParameters(par)
      case x => throw new MappingException(
                "Can't convert " + x + " to " + clazz.getSimpleName) 
    } 
  } 
  override def serialize(implicit format: Formats) = { 
    case HyperEdgeParameters(nodeIds) => Extraction.decompose(nodeIds) 
  } 
} 
/** Lift-JSON `Serializer` to serialize `WHyperEdgeParameters` to JSON arrays of the form
 * `[ ["<n1>",...,"<nn>"], <weight> ]` and reversely where `<n1>...<nn>` represent the
 * node-Ids and `<weight>` is a `JInt`.
 */
class WHyperEdgeSerializer
  extends Serializer[WHyperEdgeParameters]
  with    HyperEdgeChecker
{ 
  private val clazz = classOf[WHyperEdgeParameters] 
  override def deserialize(implicit format: Formats) = { 
    case (TypeInfo(clazz, _), json) => json match { 
      case JArray(JArray(jsonIds) :: JInt(weight) :: Nil) =>
        val par = jsonIds collect {case JString(nId) => nId}
        checkNodeIds(jsonIds, par)
        new WHyperEdgeParameters(par, weight.toLong)
      case x => throw new MappingException(
                "Can't convert " + x + " to " + clazz.getSimpleName) 
    } 
  } 
  override def serialize(implicit format: Formats) = { 
    case WHyperEdgeParameters(nodeIds, weight) =>
      JArray(Extraction.decompose(nodeIds) :: JInt(weight) :: Nil) 
  } 
} 
/** Lift-JSON `Serializer` to serialize `LHyperEdgeParameters` to JSON arrays of the form
 * `[ ["<n1>",...,"<nn>"], <label> ]` and reversely where `<n1>...<nn>` represent the
 * node-Ids and `<label>` is any subtype of `JValue`.
 * 
 * @param labelSerializers $LSERIALIZER
 */
class LHyperEdgeSerializer[L: Manifest] (labelSerializers: Serializer[L]*)
  extends LSerializer[L](labelSerializers: _*)
  with    Serializer[LHyperEdgeParameters[L]]
  with    HyperEdgeChecker
{ 
  private val clazz = classOf[LHyperEdgeParameters[L]] 
  override def deserialize(implicit format: Formats) = { 
    case (TypeInfo(clazz, _), json) => json match { 
      case JArray(JArray(jsonIds) :: jsonLabel :: Nil) =>
        val par = jsonIds collect {case JString(nId) => nId}
        checkNodeIds(jsonIds, par)
        new LHyperEdgeParameters[L](par, LabelSerialization.extract(jsonLabel))
      case x => throw new MappingException(
                "Can't convert " + x + " to " + clazz.getSimpleName) 
    } 
  } 
  override def serialize(implicit format: Formats) = { 
    case LHyperEdgeParameters(nodeIds, label) =>
      JArray(Extraction.decompose(nodeIds) ::
             LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil) 
  } 
} 
/** Lift-JSON `Serializer` to serialize `WLHyperEdgeParameters` to JSON arrays of the form
 * `[ ["<n1>",...,"<nn>"], <weight>, <label> ]` and reversely where `<n1>...<nn>`
 * represent the node-Ids, `<weight>` is a `JInt` and `<label>` is any subtype of `JValue`.
 * 
 * @param labelSerializers $LSERIALIZER
 */
class WLHyperEdgeSerializer[L: Manifest] (labelSerializers: Serializer[L]*)
  extends LSerializer[L](labelSerializers: _*)
  with    Serializer[WLHyperEdgeParameters[L]]
  with    HyperEdgeChecker
{ 
  private val clazz = classOf[WLHyperEdgeParameters[L]] 
  override def deserialize(implicit format: Formats) = { 
    case (TypeInfo(clazz, _), json) => json match { 
      case JArray(JArray(jsonIds) :: JInt(weight) :: jsonLabel :: Nil) =>
        val par = jsonIds collect {case JString(nId) => nId}
        checkNodeIds(jsonIds, par)
        new WLHyperEdgeParameters[L](par, weight.toLong, LabelSerialization.extract(jsonLabel))
      case x => throw new MappingException(
                "Can't convert " + x + " to " + clazz.getSimpleName) 
    } 
  } 
  override def serialize(implicit format: Formats) = { 
    case WLHyperEdgeParameters(nodeIds, weight, label) =>
      JArray(Extraction.decompose(nodeIds) :: JInt(weight) ::
             LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil) 
  } 
} 
