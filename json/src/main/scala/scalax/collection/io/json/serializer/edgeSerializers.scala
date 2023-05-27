package scalax.collection.io.json
package serializer

import net.liftweb.json._

import scalax.collection.io.edge._
import scalax.collection.io.edge.Types._
import scalax.collection.{OneOrMore, Several}
import error.JsonGraphError._

/** Serializes `EdgeParameters` to a JSON array of the form ["<n1>","<n2>"] and reversely
  * where <n1> and <n2> represent the node-Ids.
  */
class EdgeSerializer extends Serializer[EdgeParameters] {
  override def deserialize(implicit format: Formats) = {
    case (_, JArray(JString(n1) :: JString(n2) :: Nil))                        => EdgeParameters(n1, n2)
    case (_, JObject(JField(_, JString(n1)) :: JField(_, JString(n2)) :: Nil)) => EdgeParameters(n1, n2)
    case (TypeInfo(clazz, _), x) => throw couldNotConvertException(x, clazz)
  }

  override def serialize(implicit format: Formats) = { case EdgeParameters(nodeId_1, nodeId_2) =>
    JArray(JString(nodeId_1) :: JString(nodeId_2) :: Nil)
  }
}

/** Serializes `LEdgeParameters[Double]` to a JSON array of the form `["<n1>", "<n2>", <weight>]` and reversely
  * where `<n1>` and `<n2>` represent the node-Ids and `<weight>` a JSON number mapping to `Double`.
  */
class WEdgeSerializer extends Serializer[LEdgeParameters[Double]] {
  override def deserialize(implicit format: Formats) = {
    case (_, JArray(JString(n1) :: JString(n2) :: JDouble(weight) :: Nil)) => LEdgeParameters(n1, n2, weight)
    case (TypeInfo(clazz, _), x)                                           => throw couldNotConvertException(x, clazz)
  }

  override def serialize(implicit format: Formats) = { case LEdgeParameters(nId_1, nId_2, weight: Double) =>
    JArray(JString(nId_1) :: JString(nId_2) :: JDouble(weight) :: Nil)
  }
}

/** @define LSERIALIZER one or more lift-json custom `Serializer`s for labels.
  */
abstract class LSerializer[L: Manifest](labelSerializers: Serializer[L]*) {
  object LabelSerialization {
    implicit val labelFormats = Serialization.formats(NoTypeHints) ++ labelSerializers
    def extract(json: JValue) = json.extract[L]
    def decompose(label: L)   = Extraction.decompose(label)
  }
}

/** Serializes `LEdgeParameters` to a JSON array of the form `["<n1>", "<n2>", <label>]` and reversely
  * where `<n1>` and `<n2>` represent the node-Ids and `<label>` any JSON type mapping to `L`.
  *
  * @param labelSerializers $LSERIALIZER
  */
class LEdgeSerializer[L: Manifest](labelSerializers: Serializer[L]*)
    extends LSerializer[L](labelSerializers: _*)
    with Serializer[LEdgeParameters[L]] {

  def deserialize(implicit format: Formats) = {
    case (_, JArray(JString(n1) :: JString(n2) :: jsonLabel :: Nil)) =>
      new LEdgeParameters[L](n1, n2, LabelSerialization.extract(jsonLabel))
    case (TypeInfo(clazz, _), x) =>
      throw couldNotConvertException(x, clazz)
  }

  def serialize(implicit format: Formats) = { case LEdgeParameters(nId_1, nId_2, label) =>
    JArray(
      JString(nId_1) :: JString(nId_2) :: LabelSerialization.decompose(label.asInstanceOf[L]) :: Nil
    )
  }
}

protected trait HyperEdgeChecker {
  final protected def prepareNodes(jsonIds: List[JValue]): SeveralNodeIds =
    Several.from(jsonIds.collect { case JString(nId) => nId }) match {
      case Some(valid)                => valid
      case None if jsonIds.length < 2 => throw err(LessThan2Nodes, jsonIds.toString)
      case None                       => throw err(UnexpectedNodeId, jsonIds.toString)
    }
}

/** Serializes `HyperEdgeParameters` to a JSON array of the form `["<n1>",...,"<nn>"]` and reversely
  * where `<n1>...<nn>` represent the node-Ids.
  */
class HyperEdgeSerializer extends Serializer[HyperEdgeParameters] with HyperEdgeChecker {

  def deserialize(implicit format: Formats) = {
    case (_, JObject(JField("ends", JArray(jsonIds)) :: Nil)) =>
      HyperEdgeParameters(prepareNodes(jsonIds))
    case (TypeInfo(clazz, _), x) => throw couldNotConvertException(x, clazz)
  }

  def serialize(implicit format: Formats) = { case HyperEdgeParameters(nodeIds) =>
    JObject(JField("ends", Extraction.decompose(nodeIds.toList)))
  }
}

/** Serializes `LHyperEdgeParameters` to a JSON array of the form
  * `[ ["<n1>",...,"<nn>"], <label> ]` and reversely
  * where `<n1>...<nn>` represent the node-Ids and `<label>` is any subtype of `JValue`.
  *
  * @param labelSerializers $LSERIALIZER
  */
class LHyperEdgeSerializer[L: Manifest](labelSerializers: Serializer[L]*)
    extends LSerializer[L](labelSerializers: _*)
    with Serializer[LHyperEdgeParameters[L]]
    with HyperEdgeChecker {

  def deserialize(implicit format: Formats) = {
    case (
          _,
          JArray(
            JObject(JField("ends", JArray(jsonIds)) :: Nil) ::
            JObject(JField("labels", jsonLabel) :: Nil) ::
            Nil
          )
        ) =>
      new LHyperEdgeParameters[L](prepareNodes(jsonIds), LabelSerialization.extract(jsonLabel))
    case (TypeInfo(clazz, _), x) =>
      throw couldNotConvertException(x, clazz)
  }

  def serialize(implicit format: Formats) = { case LHyperEdgeParameters(nodeIds, label) =>
    JArray(
      JObject(JField("ends", Extraction.decompose(nodeIds.toList))) ::
        JObject(JField("labels", LabelSerialization.decompose(label.asInstanceOf[L])))
        :: Nil
    )
  }
}

protected trait DiHyperEdgeChecker {
  final protected def prepareNodes(jsonIds: List[JValue]): OneOrMoreNodeIds =
    OneOrMore.from(jsonIds.collect { case JString(nId) => nId }) match {
      case Some(validIds)          => validIds
      case None if jsonIds.isEmpty => throw err(NoNodes, jsonIds.toString)
      case None                    => throw err(UnexpectedNodeId, jsonIds.toString)
    }
}

/** Serializes `DiHyperEdgeParameters` to a JSON object of the form
  * `{ sources: ["<s1>",...,"<sn>"], targets: ["<t1>",...,"<tn>"] }` and reversely
  * where `<s?>` and `<t?>` represent the the source respectively target node-Ids.
  */
class DiHyperEdgeSerializer extends Serializer[DiHyperEdgeParameters] with DiHyperEdgeChecker {

  def deserialize(implicit format: Formats) = {
    case (
          _,
          JArray(
            JObject(JField("sources", JArray(sourceIds)) :: Nil) ::
            JObject(JField("targets", JArray(targetIds)) :: Nil) :: Nil
          )
        ) =>
      DiHyperEdgeParameters(prepareNodes(sourceIds), prepareNodes(targetIds))
    case (TypeInfo(clazz, _), x) =>
      throw couldNotConvertException(x, clazz)
  }

  def serialize(implicit format: Formats) = { case DiHyperEdgeParameters(sourceIds, targetIds) =>
    JArray(
      JObject(JField("sources", Extraction.decompose(sourceIds.toList))) ::
        JObject(JField("targets", Extraction.decompose(targetIds.toList))) ::
        Nil
    )
  }
}

/** Serializes `LDiHyperEdgeParameters` to a JSON array of the form
  * `{ sources: ["<s1>",...,"<sn>"], targets: ["<t1>",...,"<tn>"], <label> }` and reversely
  * where where `<s?>` and `<t?>` represent the the source respectively target node-Ids
  * and `<label>` is any subtype of `JValue`.
  *
  * @param labelSerializers $LSERIALIZER
  */
class LDiHyperEdgeSerializer[L: Manifest](labelSerializers: Serializer[L]*)
    extends LSerializer[L](labelSerializers: _*)
    with Serializer[LDiHyperEdgeParameters[L]]
    with DiHyperEdgeChecker {

  def deserialize(implicit format: Formats) = {
    case (
          _,
          JArray(
            JObject(JField("sources", JArray(sourceIds)) :: Nil) ::
            JObject(JField("targets", JArray(targetIds)) :: Nil) ::
            JObject(JField("labels", jsonLabel) :: Nil) ::
            Nil
          )
        ) =>
      LDiHyperEdgeParameters(
        prepareNodes(sourceIds),
        prepareNodes(targetIds),
        LabelSerialization.extract(jsonLabel)
      )
    case (TypeInfo(clazz, _), x) =>
      throw couldNotConvertException(x, clazz)
  }

  def serialize(implicit format: Formats) = { case LDiHyperEdgeParameters(sourceIds, targetIds, label) =>
    JArray(
      JObject(JField("sources", Extraction.decompose(sourceIds.toList))) ::
        JObject(JField("targets", Extraction.decompose(targetIds.toList))) ::
        JObject(JField("labels", LabelSerialization.decompose(label.asInstanceOf[L]))) ::
        Nil
    )
  }
}
