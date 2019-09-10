package scalax.collection.io.dot

import language.{postfixOps}
import scala.collection.mutable.{Set => MutableSet}
import scala.util.matching.Regex

/** Represents ''ID'' of the DOT language abstract grammar. */
sealed trait DotId

/** Verified DOT ID. */
class Id private[dot] (val id: String, val numeric: Boolean) extends DotId {
  def apply()           = id
  override def hashCode = id.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: Id => that.id == this.id
    case _        => false
  }
  override def toString = id
}
object Id {
  def apply(id: Long): Id   = new Id(id.toString, true)
  def apply(id: Double): Id = new Id(id.toString, true)

  /** Verifies 'id' and quotes it if necessary. */
  def apply(id: String): Id = {
    require(id ne null, s"'$id' was null.")
    require(id.nonEmpty, "Id was empty.")

    val len = id.length
    if (len > 1 && { val ends = (id.head, id.last); quotes exists (_ == ends) })
      new Id(id, false)
    else {
      def exactMatch(r: Regex) = r findPrefixMatchOf id exists (_.end == len)

      if (exactMatch(rNonWhitespaceString)) new Id(id, false)
      else if (exactMatch(rNumeral)) new Id(id.toString, true)
      else new Id(s""""$id"""", false)
    }
  }
  private val quotes = List(('"', '"'), ('<', '>'))
  private val rNonWhitespaceString: Regex = {
    val firstChar = "a-zA-Z\u0080-\u00ff_"
    s"[${firstChar}][${firstChar}0-9]*".r
  }
  private val rNumeral = "[-]?(.[0-9]+|[0-9]+(.[0-9]*)?)".r

  def unapply(id: Id): Option[(String, Boolean)] =
    if (id eq null) None else Some((id.id, id.numeric))
}

/** Verified DOT node_id.

    @param id The string representation of this `NodeId`.
    @param parts The individual parts of this `NodeId` such as ID and port.
  */
case class NodeId private[dot] (id: String, parts: Id*) extends DotId {

  /** The `id` of this `NodeId`. */
  def apply() = id
}
object NodeId {
  private def single(id: Id) = NodeId(id.id, id)

  def apply(id: Long): NodeId   = single(Id(id))
  def apply(id: Double): NodeId = single(Id(id))
  def apply(id: String): NodeId = single(Id(id))

  /** Builds the DOT `node_id` by concatinating all parts using colon separators. */
  def apply(part_1: Id, optional_parts: Id*): NodeId =
    if (optional_parts.isEmpty) single(part_1)
    else {
      val all = part_1 +: optional_parts
      NodeId(all map (_.id) mkString ":", all: _*)
    }
  def unapply(nodeId: NodeId): Option[(String, Seq[Id])] =
    if (nodeId eq null) None else Some((nodeId.id, nodeId.parts))
}

/** Represents constants of ''attr_stmt'' of the DOT language. */
object Elem extends Enumeration {
  type Type = Value
  val graph, node, edge = Value
}

/** Represents ''ID'' '=' ''ID'' of the DOT language grammar. */
case class DotAttrStmt(`type`: Elem.Type, attrList: Seq[DotAttr])

/** Represents ''ID'' '=' ''ID'' of the DOT language grammar. */
case class DotAttr(name: Id, value: Id)

sealed trait DotGraph {
  def id: Option[Id]
  def attrStmts: Seq[DotAttrStmt]
  def attrList: Seq[DotAttr]
  override def hashCode = id.##
  override def equals(that: Any) = that match {
    case that: DotRootGraph => this.id == that.id
    case _                  => false
  }
  def headToString: String
}

/** Represents ''graph'' of the DOT language grammar without embedded
    ''node_stmt'', ''edge_stmt'' and ''subgraph''. */
case class DotRootGraph(directed: Boolean,
                        override val id: Option[Id],
                        strict: Boolean = false,
                        override val attrStmts: Seq[DotAttrStmt] = Nil,
                        override val attrList: Seq[DotAttr] = Nil)
    extends DotGraph {
  def headToString = {
    val res = new StringBuilder(32)
    if (strict) res append "strict "
    if (directed) res append "di"
    res append s"graph ${id map (_ + " ") getOrElse ""}{"
    res.toString
  }
  override def toString = headToString + attrList.mkString + " }"
}

/** Represents ''subgraph'' of the DOT language grammar without embedded
    ''node_stmt'', ''edge_stmt'' and ''subgraph''. */
case class DotSubGraph(ancestor: DotGraph,
                       subgraphId: Id,
                       override val attrStmts: Seq[DotAttrStmt] = Nil,
                       override val attrList: Seq[DotAttr] = Nil)
    extends DotGraph {
  val id           = Some(subgraphId)
  def headToString = "subgraph %s {".format(id get)
}

/** Represents ''stmt'' of the DOT language grammar.
  *
  *  @define EXCLSUB with the exception of ''subgraph'' which is covered by the return
  *          value of the user-supplied `*Transformer` functions passed to `toDot`.
  */
sealed trait DotStmt

/** Represents ''node_stmt'' of the DOT language grammar $EXCLSUB. */
case class DotNodeStmt(nodeId: NodeId, attrList: Seq[DotAttr] = Seq()) extends DotStmt

/** Represents ''edge_stmt'' of the DOT language grammar $EXCLSUB. */
case class DotEdgeStmt(node_1Id: NodeId, node_2Id: NodeId, attrList: Seq[DotAttr] = Seq()) extends DotStmt

protected[dot] case class DotCluster(dotGraph: DotGraph, dotStmts: MutableSet[DotStmt] = MutableSet()) {
  override def hashCode = dotGraph.id.##
  override def equals(that: Any) = that match {
    case that: DotCluster => this.dotGraph.id == that.dotGraph.id
    case _                => false
  }
}
