package scalax.collection
package immutable

import language.higherKinds

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.mutable.ArraySet

/** Implements an incident list based immutable graph representation.
  *
  * @author Peter Empen
  */
trait AdjacencyListGraph[
    N, E[X] <: EdgeLike[X], +This[X, Y[X] <: EdgeLike[X]] <: AdjacencyListGraph[X, Y, This] with Graph[X, Y]]
    extends GraphLike[N, E, This]
    with AdjacencyListBase[N, E, This] { selfGraph: This[N, E] =>

  type NodeT <: InnerNodeImpl
  abstract class InnerNodeImpl(value: N, hints: ArraySet.Hints) extends NodeBase(value) with InnerNode { this: NodeT =>

    final override val edges: ArraySet[EdgeT]                      = ArraySet.emptyWithHints[EdgeT](hints)
    @transient protected var _diSuccessors: immutable.EqSet[NodeT] = _
    final def diSuccessors: Set[NodeT] = {
      if (_diSuccessors eq null) _diSuccessors = new immutable.EqSet(Adj.diSucc)
      _diSuccessors
    }
  }

  type NodeSetT = NodeSet
  class NodeSet extends super.NodeSet {
    @inline final override protected def minus(node: NodeT) { coll -= node }
    def +(node: NodeT) =
      if (coll contains node) this
      else { val c = copy; c.coll += node; c }

    protected[AdjacencyListGraph] def add(edge: EdgeT): Boolean = {
      var added = false
      edge.ends foreach { n =>
        val inColl = coll findElem n getOrElse { coll += n; n }
        added = (inColl add edge) || added
      }
      added
    }

    protected[collection] def +=(edge: EdgeT): this.type = { add(edge); this }
  }
  override def nodes: NodeSetT

  @inline final protected def newEdgeTArray(size: Int): Array[EdgeT] = new Array[EdgeT](size)

  type EdgeSetT = EdgeSet
  class EdgeSet extends super.EdgeSet {
    override protected[collection] def initialize(edges: Traversable[E[N]]): Unit =
      if (edges ne null)
        edges foreach (this += Edge(_))

    @inline final protected[AdjacencyListGraph] def +=(edge: EdgeT): this.type = {
      if (nodes add edge) statistics(edge, plus = true)
      this
    }

    @inline final protected[immutable] def addEdge(edge: EdgeT) { +=(edge) }
    @inline final def +(edge: EdgeT): Set[EdgeT] = toSet + edge
    @inline final def -(edge: EdgeT): Set[EdgeT] = toSet - edge

    @inline final override lazy val maxArity        = super.maxArity
    @inline final override lazy val hasAnyMultiEdge = super.hasAnyMultiEdge
  }
  override def edges: EdgeSetT

  def copy(nodes: Traversable[N], edges: Traversable[E[N]]): This[N, E]

  def +(n: N) =
    if (nodes contains Node(n)) this
    else copy(nodes.toOuter.toBuffer += n, edges.toOuter)

  def +(e: E[N]) =
    if (edges contains Edge(e)) copy(nodes.toOuter, edges.toOuter.toBuffer -= e)
    else this

  def -(n: N) = nodes find (nf => nf.value == n) match {
    case Some(nf) => copy(nodes.toOuter.toBuffer -= n, edges.toOuter.toBuffer --= (nf.edges map (_.toOuter)))
    case None     => this
  }

  protected def -(e: E[N]) =
    if (edges contains Edge(e)) copy(nodes.toOuter, edges.toOuter.toBuffer -= e)
    else this
}
