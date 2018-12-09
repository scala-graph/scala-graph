package scalax.collection
package immutable

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.mutable.ArraySet

/** Implements an incident list based immutable graph representation.
  *
  * @author Peter Empen
  */
trait AdjacencyListGraph[
    N, E[X] <: EdgeLike[X], +This[X, Y[X] <: EdgeLike[X]] <: AdjacencyListGraph[X, Y, This] with Graph[X, Y]]
    extends GraphLike[N, E, This]
    with GraphOps[N, E, This]
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
        edges foreach (this += InnerEdge(_))

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

  /** Creates a new supergraph with an additional node unless this graph contains `node`. */
  def +(node: N): This[N, E] =
    if (this contains node) this
    else copy(nodes.toOuter.toBuffer += node, edges.toOuter)

  /** Creates a new supergraph with an additional edge unless this graph contains `edge`. */
  def +(edge: E[N]): This[N, E] =
    if (this contains edge) copy(nodes.toOuter, edges.toOuter.toBuffer -= edge)
    else this

  /** Creates a new graph with the elements of this graph plus the passed elements. */
  final def ++(nodes: Traversable[N], edges: Traversable[E[N]], elems: Traversable[Graph[N, E]#InnerElem]): This[N, E] =
    bulkOp(nodes, edges, elems, plusPlus)

  /** Creates a new graph with the elements of this graph minus `node` and its incident edges. */
  def -(node: N): This[N, E] = nodes find (nf => nf.value == node) match {
    case Some(nf) => copy(nodes.toOuter.toBuffer -= node, edges.toOuter.toBuffer --= (nf.edges map (_.toOuter)))
    case None     => this
  }

  /** Creates a new graph with the elements of this graph minus `edge`. */
  def -(edge: E[N]): This[N, E] =
    if (this contains edge) copy(nodes.toOuter, edges.toOuter.toBuffer -= edge)
    else this

  /** Creates a new graph with the elements of this graph minus the passed elements. */
  final def --(nodes: Traversable[N], edges: Traversable[E[N]], elems: Traversable[Graph[N, E]#InnerElem]): This[N, E] =
    bulkOp(nodes, edges, elems, minusMinus)

  /** Prepares and calls `plusPlus` or `minusMinus`. */
  final protected def bulkOp(nodes: Traversable[N],
                             edges: Traversable[E[N]],
                             elems: Traversable[Graph[N, E]#InnerElem],
                             op: (Iterator[N @uV], Iterator[E[N]]) => This[N, E] @uV): This[N, E] = {
    val (nodeElems, edgeElems) = partition(elems)
    op(nodes.toIterator ++ nodeElems.toIterator, edges.toIterator ++ edgeElems.toIterator)
  }

  /** Implements the heart of `++` calling the `from` factory method of the companion object.
    *  $REIMPLFACTORY */
  final protected def plusPlus(newNodes: Iterator[N], newEdges: Iterator[E[N]]): This[N, E] =
    companion.from[N, E](nodes.toOuter ++ newNodes, edges.toOuter ++ newEdges)

  /** Implements the heart of `--` calling the `from` factory method of the companion object.
    *  $REIMPLFACTORY */
  final protected def minusMinus(delNodes: Iterator[N], delEdges: Iterator[E[N]]): This[N, E] = {
    val delNodesEdges = minusMinusNodesEdges(delNodes, delEdges)
    companion.from[N, E](delNodesEdges._1, delNodesEdges._2)
  }

  /** Calculates the `nodes` and `edges` arguments to be passed to a factory method
    *  when delNodes and delEdges are to be deleted by `--`.
    */
  final protected def minusMinusNodesEdges(delNodes: Iterator[N], delEdges: Iterator[E[N]]) =
    (nodes.toOuter -- delNodes, {
      val delNodeSet = delNodes.toSet
      val restEdges =
        for (e <- edges.toOuter if e.ends forall (n => !(delNodeSet contains n))) yield e
      restEdges -- delEdges
    })
}
