package scalax.collection.io.json
package error

trait JsonGraphIssue
object JsonGraphError extends Enumeration with JsonGraphIssue {
  type JsonGraphError = Value
  val NonObjArrValue, NonArray, InvalidElemTypeId, EmptyNodeFieldList, LessThan2Nodes, NoNodes, UnexpectedNodeId,
      UnexpectedDescr, UnknownNode, NoNodeDescr, NoEdgeDescr = Value

  private val messages: Map[JsonGraphError, String] = Map(
    NonObjArrValue -> """"{}" is of JSON type {}. Values of nodes/edges JSON fields must be of type JSON object or array.""",
    NonArray -> """"{}" is of JSON type {}. Nodes and edges must be JSON arrays.""",
    InvalidElemTypeId -> """The node/edgeTypeId "{}" found in the JSON text is not contained in the descriptor's node/edgeDescriptors.""",
    EmptyNodeFieldList -> """Empty node field list detected. Node field lists must contain at leas one field.""",
    LessThan2Nodes     -> """Hyperedge with less than two nodes: "{}".""",
    NoNodes            -> """Directed hyperedge with empty source or target: "{}".""",
    UnexpectedNodeId   -> """Node-Ids contain non-string value(s): "{}".""",
    UnexpectedDescr    -> """Edge-descriptor "{}" of unexpected type cannot be processed.""",
    UnknownNode        -> """Edge cannot be created due to missing node with id "{}".""",
    NoNodeDescr        -> """No 'NodeDescriptor' capable of processing type "{}" found.""",
    NoEdgeDescr        -> """No 'EdgeDescriptor' capable of processing type "{}" found."""
  )

  def err(errType: JsonGraphError, args: String*): JsonGraphException =
    JsonGraphException(errType, replacePlaceholders(messages(errType), args))

  case class JsonGraphException(err: JsonGraphError, msg: String) extends Exception("JSON-Graph error: " + msg)
}
object JsonGraphWarning extends Enumeration with JsonGraphIssue {
  type JsonGraphWarning = Value
  val DuplicateNodeId = Value

  def warn(warnType: JsonGraphWarning, args: String*) =
    warnType match {
      case m if m == DuplicateNodeId =>
        """Duplicate node id "{}" returned for noe "{}". Node "{}" has the same id."""
    }
}
