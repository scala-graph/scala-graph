package scalax.collection.io.json
package error

trait JsonGraphIssue
object JsonGraphError extends Enumeration with JsonGraphIssue {
  type JsonGraphError = Value
  val NonObjArrValue,
      NonArray,
      ObjectSizeNEQ1,
      InvalidElemTypeId,
      EmptyNodeFieldList,
      InsufficientNodes,
      UnexpectedNodeId,
      UnexpectedDescr,
      UnknownNode,
      NoNodeDescr,
      NoEdgeDescr = Value

  def err(errType: JsonGraphError, args: String*) = {
    val msg = errType match {
      case m if m == NonObjArrValue    =>
           """"{}" is of JSON type {}. Values of nodes/edges JSON fields must be of type JSON object or array."""
      case m if m == NonArray          =>
           """"{}" is of JSON type {}. Nodes and edges must be JSON arrays.""" 
      case m if m == ObjectSizeNEQ1    =>
           """Typed edge JSON objects must contain exactly one field.""" 
      case m if m == InvalidElemTypeId =>
           """The node/edgeTypeId "{}" found in the JSON text is not contained in the descriptor's node/edgeDescriptors."""
      case m if m == EmptyNodeFieldList=>
           """Empty node field list detected. Node field lists must contain at leas one field."""
      case m if m == InsufficientNodes =>
           """Hyperedge with less than two nodes detected: "{}"."""
      case m if m == UnexpectedNodeId  =>
           """JSON array of hyperedge node-Ids contains non-string value(s): "{}"."""
      case m if m == UnexpectedDescr  =>
           """Edge-descriptor "{}" of unexpected type cannot be processed."""
      case m if m == UnknownNode       =>
           """Edge cannot be created due to missing node with id "{}"."""
      case m if m == NoNodeDescr       =>
           """No 'NodeDescriptor' capable of processing type "{}" found."""
      case m if m == NoEdgeDescr       =>
           """No 'EdgeDescriptor' capable of processing type "{}" found."""
    }
    val replMsg = replacePlaceholders(msg, args)
    JsonGraphException(errType, replMsg)
  }
  case class JsonGraphException(val err: JsonGraphError, val msg: String)
    extends Exception("JSON-Graph error: " + msg)
}
object JsonGraphWarning extends Enumeration with JsonGraphIssue {
  type JsonGraphWarning = Value
  val DuplicateNodeId = Value

  def warn(warnType: JsonGraphWarning, args: String*) =
    warnType match {
      case m if m == DuplicateNodeId   =>
           """Duplicate node id "{}" returned for noe "{}". Node "{}" has the same id."""
  }
}