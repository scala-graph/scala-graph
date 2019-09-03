package scalax.collection
package immutable

import language.higherKinds

import GraphPredef.EdgeLikeIn
import GraphEdge.OrderedEndpoints
import mutable.ArraySet

/** Implements an incident list based immutable graph representation.
  *
  * @author Peter Empen
  */
trait AdjacencyListGraph[
    N, E[+X] <: EdgeLikeIn[X], +This[X, Y[+X] <: EdgeLikeIn[X]] <: AdjacencyListGraph[X, Y, This] with Graph[X, Y]]
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
    @inline final override protected def minus(node: NodeT) { collection -= node }
    override def +(node: NodeT) =
      if (collection contains node) this
      else { val c = copy; c.collection += node; c }

    protected[AdjacencyListGraph] def add(edge: EdgeT): Boolean = {
      var added = false
      edge foreach { n =>
        val inColl = coll findElem n getOrElse { collection += n; n }
        added = (inColl add edge) || added
      }
      added
    }

    protected[collection] def +=(edge: EdgeT): this.type = { add(edge); this }
  }
  override def nodes: NodeSetT

  type EdgeT = EdgeBase
  @inline final protected def newEdgeTArray(size: Int): Array[EdgeT] = new Array[EdgeT](size)
  final override protected def newEdge(innerEdge: E[NodeT]): EdgeT =
    if (innerEdge.isInstanceOf[OrderedEndpoints]) new EdgeT(innerEdge) with OrderedEndpoints
    else new EdgeT(innerEdge)

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
    @inline final def addOne(edge: EdgeT): Set[EdgeT] = toSet + edge
    @inline final def minusOne(edge: EdgeT): Set[EdgeT] = toSet - edge

    @inline final override lazy val maxArity        = super.maxArity
    @inline final override lazy val hasAnyMultiEdge = super.hasAnyMultiEdge
  }
  override def edges: EdgeSetT

  protected def copy(nodes: Traversable[N], edges: Traversable[E[N]]): This[N, E]

  def +(n: N) =
    if (nodes contains Node(n)) this
    else copy(nodes.toOuter.toBuffer += n, edges.toOuter)

  def -(n: N) = nodes find (nf => nf.value == n) match {
    case Some(nf) => copy(nodes.toOuter.toBuffer -= n, edges.toOuter.toBuffer --= (nf.edges map (_.toOuter)))
    case None     => this
  }

  def minusIsolated(n: N) = nodes find n match {
    case Some(nf) =>
      val newNodes = nodes.toOuter.toBuffer
      val newEdges = edges.toOuter.toBuffer
      nodes.subtract(nf, false, nf => newNodes -= n, nf => newEdges --= (nf.edges map (_.toOuter)))
      copy(newNodes, newEdges)
    case None => this
  }

  protected def -!#(e: E[N]) = edges find (ef => ef == e) match {
    case Some(ef) => copy(nodes.toOuter.toBuffer --= ef.privateNodes map (n => n.value), edges.toOuter.toBuffer -= e)
    case None     => this
  }
}
