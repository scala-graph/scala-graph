package scalax.collection
package mutable

import language.higherKinds

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.immutable.AdjacencyListBase

/** Implements an incident list based mutable graph representation.
  *
  * @author Peter Empen
  */
trait AdjacencyListGraph[
    N, E <: EdgeLike[N], +This[X, Y <: EdgeLike[X]] <: AdjacencyListGraph[X, Y, This] with Graph[X, Y]]
    extends GraphLike[N, E, This]
    with AdjacencyListBase[N, E, This] {
  selfGraph: This[N, E] =>

  type NodeT <: InnerNodeImpl
  abstract class InnerNodeImpl(outer: N, hints: ArraySet.Hints)
      extends NodeBase(outer)
      with super[GraphLike].InnerNode
      with InnerNode { this: NodeT =>

    final override val edges: ArraySet[EdgeT] = ArraySet.emptyWithHints[EdgeT](hints)
    import Adj._

    final override protected[collection] def add(edge: EdgeT): Boolean =
      if (super.add(edge)) {
        if (selfGraph.edges.initialized) addDiSuccOrHook(edge)
        true
      } else false

    protected[AdjacencyListGraph] def upsert(edge: EdgeT): Boolean = {
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

    protected[AdjacencyListGraph] def remove(edge: EdgeT): Boolean =
      if (edges.remove(edge)) {
        if (selfGraph.edges.initialized) {

          def onLooping(): Unit =
            edges.find((e: EdgeT) => e.isLooping).fold(ifEmpty = _aHook = None)((e: EdgeT) => _aHook = Some(this -> e))

          def onNonLooping(): Unit = edge.targets foreach (t =>
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

  type NodeSetT = NodeSet
  class NodeSet extends super[GraphLike].NodeSet with super.NodeSet {
    override def add(node: NodeT): Boolean       = coll add node
    @inline final def +=(node: NodeT): this.type = { add(node); this }

    protected[collection] def add(edge: EdgeT): Boolean = {
      var someNew = false
      edge.ends foreach { n =>
        val inColl = coll findElem n getOrElse { coll += n; n }
        someNew = (inColl add edge) || someNew
      }
      someNew
    }

    protected[collection] def +=(edge: EdgeT): this.type = { add(edge); this }

    protected[collection] def upsert(edge: EdgeT): Boolean = {
      var someNew = false
      edge.ends foreach { n =>
        val inColl = coll findElem n getOrElse { coll += n; n }
        someNew = (inColl upsert edge) || someNew
      }
      someNew
    }

    protected[collection] def remove(edge: EdgeT): Boolean =
      edge.ends.toSet forall (n => (coll findElem n) exists (_ remove edge))

    protected[collection] def -=(edge: EdgeT): this.type = { remove(edge); this }
    override protected def minus(node: NodeT): Unit      = coll -= node
    override protected def minusEdges(node: NodeT): Unit =
      // toList is necessary to avoid failure of -=(node) like in TEdit.test_MinusEq_2
      edges --= node.edges.toList
  }
  override def nodes: NodeSetT

  @inline final protected def newEdgeTArray(size: Int): Array[EdgeT] = new Array[EdgeT](size)

  type EdgeSetT = EdgeSet
  class EdgeSet extends super[GraphLike].EdgeSet with super.EdgeSet {
    protected[AdjacencyListGraph] var initialized = false

    override protected[collection] def initialize(edges: Traversable[E]): Unit = {
      if (edges ne null)
        edges foreach (this add InnerEdge(_))
      initialized = true
    }

    override def add(edge: EdgeT): Boolean =
      if (nodes add edge) statistics(edge, plus = true) else false

    @inline final protected[collection] def addEdge(edge: EdgeT): Unit = add(edge)

    def upsert(edge: EdgeT): Boolean =
      if (nodes upsert edge) statistics(edge, plus = true) else false

    override def remove(edge: EdgeT): Boolean =
      if (nodes remove edge) statistics(edge, plus = false) else false

    def removeWithNodes(edge: EdgeT) = {
      val privateNodes = edge.privateNodes
      if (remove(edge)) {
        nodes --= privateNodes
        true
      } else false
    }

    @inline final override def maxArity: Int = super.maxArity
  }
  override def edges: EdgeSetT

  @inline final def clear(): Unit            = nodes.clear()
  @inline final def add(node: N): Boolean    = nodes add Node(node)
  @inline final def add(edge: E): Boolean    = edges add InnerEdge(edge)
  @inline final def upsert(edge: E): Boolean = edges upsert InnerEdge(edge)
}
