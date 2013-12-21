package scalax.collection.io.dot

import language.{postfixOps}
import scala.collection.mutable.{Set => MutableSet}

sealed trait DotGraph {
  def id: Option[String]
  def kvList: Seq[DotAttr]
  override def hashCode = id.##
  override def equals(that: Any) = that match {
    case that: DotRootGraph => this.id == that.id
    case _ => false
  }
  def headToString: String
}
/** Represents ''graph'' of the DOT language syntax without its embedded elements. */
case class DotRootGraph(directed:            Boolean,
                        override val id:     Option[String],
                        strict:              Boolean = false,
                        override val kvList: Seq[DotAttr] = Seq()) extends DotGraph {
  def headToString = {
    val res = new StringBuilder(32)
    if (strict)   res append "strict "
    if (directed) res append "di"
    res append "graph %s{".format(id map (_ + " ") getOrElse "")
    res.toString
  }
  override def toString = headToString + kvList.mkString + " }"
}
/** Represents ''subgraph'' of the DOT language syntax without its embedded elements. */
case class DotSubGraph(ancestor:            DotGraph,
                       subgraphId:          String,
                       override val kvList: Seq[DotAttr] = Seq()) extends DotGraph {
  val id = Some(subgraphId)
  def headToString = "subgraph %s {".format(id get)
}
/** Represents ''stmt'' of the DOT language syntax $EXCLSUB.
 *
 *  @define EXCLSUB with the exception of ''subgraph'' which is covered by the return
 *          value of the user-supplied `*Transformer` functions passed to `toDot`. 
 */
trait DotStmt
/** Represents ''node_stmt'' of the DOT language syntax $EXCLSUB. */
case class DotNodeStmt(nodeId: String,
                       attrList: Seq[DotAttr] = Seq()) extends DotStmt
/** Represents ''edge_stmt'' of the DOT language syntax. */
case class DotEdgeStmt(node_1Id: String, node_2Id: String,
                       attrList: Seq[DotAttr] = Seq()) extends DotStmt
/** Represents ''ID'' '=' ''ID'' of the DOT language syntax. */
case class DotAttr(name: String, value: String)

protected[dot] case class DotCluster(dotGraph: DotGraph,
                                     dotStmts: MutableSet[DotStmt] = MutableSet()) {
  override def hashCode = dotGraph.id.##
  override def equals(that: Any) = that match {
    case that: DotCluster => this.dotGraph.id == that.dotGraph.id
    case _ => false
  }
}

