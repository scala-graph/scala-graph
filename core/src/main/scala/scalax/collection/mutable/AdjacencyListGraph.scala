package scalax.collection
package mutable

import language.higherKinds

import GraphPredef.EdgeLikeIn
import GraphEdge.OrderedEndpoints
import immutable.AdjacencyListBase

/** Implements an incident list based mutable graph representation.
  *
  * @author Peter Empen
  */
trait AdjacencyListGraph[
    N, E[+X] <: EdgeLikeIn[X], +This[X, Y[+X] <: EdgeLikeIn[X]] <: AdjacencyListGraph[X, Y, This] with Graph[X, Y]]
    extends GraphLike[N, E, This]
    with AdjacencyListBase[N, E, This] {
  selfGraph: This[N, E] =>

  type NodeT <: InnerNodeImpl
  abstract class InnerNodeImpl(value: N, hints: ArraySet.Hints)
      extends NodeBase(value)
      with super[GraphLike].InnerNode
      with InnerNode { this: NodeT =>

    final override val edges: ArraySet[EdgeT] = ArraySet.emptyWithHints[EdgeT](hints)
    import Adj._

    final override protected[collection] def add(edge: EdgeT): Boolean =
      if (super.add(edge)) {
        if (selfGraph.edges.initialized) addDiSuccOrHook(edge)
        true
      } else false

    protected[collection] def upsert(edge: EdgeT): Boolean = {
      val inserted = edges upsert edge
      if (selfGraph.edges.initialized) addDiSuccOrHook(edge)
      inserted
    }

    final protected def addDiSuccOrHook(edge: EdgeT) {
      if (edge.matches(nodeEqThis, nodeEqThis) && aHook.isEmpty)
        _aHook = Some(this -> edge)
      addDiSuccessors(edge, (n: NodeT) => diSucc put (n, edge))
    }

    final def diSuccessors: Set[NodeT] = new immutable.EqSet(diSucc)

    protected[collection] def remove(edge: EdgeT): Boolean =
      if (edges.remove(edge)) {
        if (selfGraph.edges.initialized) {

          def onLooping(): Unit =
            edges.find((e: EdgeT) => e.isLooping).fold(ifEmpty = _aHook = None)((e: EdgeT) => _aHook = Some(this -> e))

          def onNonLooping(): Unit = edge withTargets (t =>
            edges
              .find((e: EdgeT) => e.hasTarget((n: NodeT) => n eq t))
              .fold[Unit](ifEmpty = diSucc remove t)((e: EdgeT) => if (e hasSource this) diSucc put (t, e)))

          if (edge.isHyperEdge)
            if (edge.isLooping) {
              onLooping()
              onNonLooping()
            } else onNonLooping()
          else if (edge.isLooping) onLooping()
          else onNonLooping()
        }
        true
      } else false
  }

  type NodeSetT <: NodeSet
  class NodeSet extends super[GraphLike].NodeSet with super.NodeSet {
    @inline override def add(node: NodeT): Boolean               = collection add node
    final protected[collection] def add(edge: EdgeT): Boolean    = fold(edge, (_: NodeT).add)
    final protected[collection] def upsert(edge: EdgeT): Boolean = fold(edge, (_: NodeT).upsert)

    private def fold(edge: EdgeT, op: NodeT => EdgeT => Boolean): Boolean =
      edge.foldLeft(false) {
        case (cum, n) =>
          op(collection findElem n getOrElse { collection += n; n })(edge) || cum
      }

    final protected[collection] def remove(edge: EdgeT): Boolean =
      edge.nodes.toSet forall (n => (collection findElem n) exists (_ remove edge))

    @inline final protected[collection] def +=(edge: EdgeT): this.type = { add(edge); this }
    @inline final def addOne(node: NodeT): this.type                   = { add(node); this }

    @inline final protected[collection] def -=(edge: EdgeT): this.type = { remove(edge); this }

    final protected def minus(node: NodeT): Unit = collection -= node
    final protected def minusEdges(node: NodeT): Unit =
      edges --= node.edges.toList // TODO toList is necessary to avoid failure of -=(node) like in TEdit.test_MinusEq_2
  }
  override def nodes: NodeSetT

  type EdgeT = EdgeImpl

  @inline final def newEdgeTArray(size: Int): Array[EdgeT] = new Array[EdgeT](size)

  @SerialVersionUID(7972L)
  class EdgeImpl(override val edge: E[NodeT]) extends EdgeBase(edge) {
    def remove: Boolean = edges remove this

    def removeWithNodes(edge: E[N]): Boolean =
      if (edges remove this) {
        selfGraph.nodes --= privateNodes; true
      } else false
  }

  @inline final override protected def newEdge(innerEdge: E[NodeT]): EdgeT =
    if (innerEdge.isInstanceOf[OrderedEndpoints]) new EdgeT(innerEdge) with OrderedEndpoints
    else new EdgeT(innerEdge)

  type EdgeSetT <: EdgeSet
  class EdgeSet extends super[GraphLike].EdgeSet with super.EdgeSet {
    final protected[AdjacencyListGraph] var initialized = false

    final override protected[collection] def initialize(edges: Traversable[E[N]]): Unit = {
      if (edges ne null)
        edges foreach (this add Edge(_))
      initialized = true
    }

    override def add(edge: EdgeT): Boolean    = if (nodes add edge) statistics(edge, plus = true) else false
    final def upsert(edge: EdgeT): Boolean    = if (nodes upsert edge) statistics(edge, plus = true) else false
    override def remove(edge: EdgeT): Boolean = if (nodes remove edge) statistics(edge, plus = false) else false

    def removeWithNodes(edge: EdgeT): Boolean =
      if (remove(edge)) {
        nodes --= edge.privateNodes
        true
      } else false

    @inline final override def maxArity: Int = super.maxArity
  }
  override def edges: EdgeSetT

  @inline final def clear: Unit                          = nodes.clear
  @inline final def add(node: N): Boolean                = nodes add Node(node)
  @inline final def add(edge: E[N]): Boolean             = edges add Edge(edge)
  @inline final protected def +=#(edge: E[N]): this.type = { add(edge); this }
  @inline final def upsert(edge: E[N]): Boolean          = edges upsert Edge(edge)
}
