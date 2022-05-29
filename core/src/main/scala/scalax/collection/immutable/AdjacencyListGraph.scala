package scalax.collection
package immutable

import scalax.collection.{Graph => AnyGraph}
import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.mutable.ArraySet

/** Implements an incident list based immutable graph representation.
  * @author Peter Empen
  */
trait AdjacencyListGraph[N, E <: EdgeLike[N], +This[X, Y <: EdgeLike[X]] <: AdjacencyListGraph[X, Y, This] with Graph[
  X,
  Y
]] extends GraphLike[N, E, This]
    with GraphOps[N, E, This]
    with AdjacencyListBase[N, E, This] { selfGraph: This[N, E] =>

  type NodeT <: InnerNodeImpl
  abstract class InnerNodeImpl(val outer: N, hints: ArraySet.Hints) extends NodeBase with AdjacendyListBaseInnerNode {
    this: NodeT =>

    final override val edges: ArraySet[EdgeT]                      = ArraySet.emptyWithHints[EdgeT](hints)
    @transient protected var _diSuccessors: immutable.EqSet[NodeT] = _

    final def diSuccessors: Set[NodeT] = {
      if (_diSuccessors eq null) _diSuccessors = new immutable.EqSet(Adj.diSucc)
      _diSuccessors
    }
  }

  type NodeSetT = AdjacencyListNodeSet
  class AdjacencyListNodeSet extends AdjacencyListBaseNodeSet {
    @inline final override protected def minus(node: NodeT): Unit = collection -= node
    override def +(node: NodeT) =
      if (collection contains node) this
      else { val c = copy; c.collection += node; c }

    protected[AdjacencyListGraph] def add(edge: EdgeT): Boolean = {
      var added = false
      edge.ends foreach { n =>
        val inColl = collection findElem n getOrElse { collection += n; n }
        added = (inColl add edge) || added
      }
      added
    }

    protected[collection] def +=(edge: EdgeT): this.type = { add(edge); this }
  }
  override def nodes: NodeSetT

  @inline final protected def newEdgeTArray(size: Int): Array[EdgeT] = new Array[EdgeT](size)

  type EdgeSetT = AdjacencyListEdgeSet
  class AdjacencyListEdgeSet extends AdjacencyListBaseEdgeSet {
    override protected[collection] def initialize(edges: Iterable[E]): Unit =
      if (edges ne null)
        edges foreach (this += BaseInnerEdge(_))

    @inline final protected[AdjacencyListGraph] def +=(edge: EdgeT): this.type = {
      if (nodes add edge) statistics(edge, plus = true)
      this
    }

    @inline final protected[immutable] def addEdge(edge: EdgeT): Unit = +=(edge)
    @inline final def incl(edge: EdgeT)                               = toSet + edge
    @inline final def excl(edge: EdgeT)                               = toSet - edge

    @inline final override lazy val maxArity        = super.maxArity
    @inline final override lazy val hasAnyMultiEdge = super.hasAnyMultiEdge
  }
  override def edges: EdgeSetT

  protected def copy(nodes: Iterable[N], edges: Iterable[E]): This[N, E]

  def incl(node: N): This[N, E] =
    if (this contains node) this
    else copy(nodes.outerIterable concat Iterable(node), edges.toOuter)

  def incl(edge: E): This[N, E] =
    if (this contains edge) this
    else copy(nodes.outerIterable, edges.outerIterable concat Iterable(edge))

  def excl(node: N): This[N, E] =
    nodes find (nf => nf.outer == node) match {
      case Some(nf) =>
        val exclEdges = nf.edges map (_.outer)
        copy(
          nodes.outerIterable.filterNot(_ == node),
          edges.outerIterable.filterNot(exclEdges.contains)
        )
      case None => this
    }

  def excl(edge: E): This[N, E] =
    if (this contains edge) copy(nodes.outerIterable, edges.outerIterable.filterNot(_ == edge))
    else this

  final def --(nodes: Iterable[N], edges: Iterable[E]): This[N, E] = bulkOp(nodes, edges, minusMinus)

  @inline final def --(that: AnyGraph[N, E]): This[N, E] = this diff that
}
