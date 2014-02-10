package scalax.collection
package mutable

import language.higherKinds
import collection.Set

import GraphPredef.EdgeLikeIn
import immutable.AdjacencyListBase

/** Implements an incident list based mutable graph representation.
 *   
 * @author Peter Empen
 */
trait AdjacencyListGraph[N,
                         E[X] <: EdgeLikeIn[X],
                        +This[X, Y[X]<:EdgeLikeIn[X]]
                              <: AdjacencyListGraph[X,Y,This] with Graph[X,Y]]
  extends GraphLike[N,E,This]
  with    AdjacencyListBase[N,E,This]
{ selfGraph: This[N,E] =>
  type NodeT <: InnerNodeImpl 
  abstract class InnerNodeImpl(value: N, hints: ArraySet.Hints)
    extends NodeBase(value)
    with    super[GraphLike].InnerNodeLike
    with    InnerNodeLike
  { this: NodeT =>
    final override val edges: ArraySet[EdgeT] = ArraySet.emptyWithHints[EdgeT](hints)
    import Adj._

    protected[AdjacencyListGraph] def add(edge: EdgeT): Boolean =
      if (edges.add(edge)) {
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
        aHook = Some(this -> edge)
      addDiSuccessors(edge, (n: NodeT) => diSucc put (n, edge))
    }

    import EqMap._
    final def diSuccessors: Set[NodeT] = diSucc.toKeySet
    
    protected[AdjacencyListGraph] def remove(edge: EdgeT): Boolean =
      if (edges.remove(edge)) {
        if (selfGraph.edges.initialized) {
          
          def onLooping: Unit = edges.find((e: EdgeT) => e.isLooping).fold(
            ifEmpty = aHook = None)((e: EdgeT) => aHook = Some(this -> e))
            
          def onNonLooping: Unit = edge withTargets (t =>
            edges.find((e: EdgeT) => e.hasTarget((n: NodeT) => n eq t)).fold(
              ifEmpty = diSucc remove t)((e: EdgeT) => diSucc put (t, e)))
          
          if (edge.isHyperEdge)
            if (edge.isLooping) {
              onLooping
              onNonLooping
            } else onNonLooping
          else if (edge.isLooping) onLooping
          else onNonLooping
        }
        true
      } else false
  }

  type NodeSetT = NodeSet
  @SerialVersionUID(7970L)
  class NodeSet extends super[GraphLike].NodeSet with super.NodeSet
  {
    override def add(node: NodeT) = coll add node
    @inline final def += (node: NodeT): this.type	= { add(node); this }
    protected[collection] def add(edge: EdgeT): Boolean = {
      var someNew = false
      edge foreach { n =>
        val inColl = coll findEntry n getOrElse {coll += n; n}
        someNew = (inColl add edge) || someNew
      }
      someNew
    }
    protected[collection] def += (edge: EdgeT): this.type	= { add(edge); this }
    protected[collection] def upsert(edge: EdgeT): Boolean = {
      var someNew = false
      edge foreach { n =>
        val inColl = coll findEntry n getOrElse {coll += n; n}
        someNew = (inColl upsert edge) || someNew
      }
      someNew
    }
    protected[collection] def remove (edge: EdgeT): Boolean =
      edge.nodes.toSet forall (n => (coll findEntry n) exists (_ remove edge))
    protected[collection] def -= (edge: EdgeT): this.type = { remove(edge); this }
    override protected def minus     (node: NodeT) { coll -= node }
    override protected def minusEdges(node: NodeT) { 
      // toList is necessary to avoid failure of -=(node) like in TEdit.test_MinusEq_2
      edges --= node.edges.toList
    }
  }
  override val nodes: NodeSetT

  type EdgeT = EdgeImpl
  @inline final def newEdgeTArray(size: Int): Array[EdgeT] = new Array[EdgeT](size)
  @SerialVersionUID(7972L)
  class EdgeImpl(override val edge: E[NodeT]) extends EdgeBase(edge)
  {
    def remove: Boolean = edges remove this
    def removeWithNodes(edge: E[N]) = if (edges remove this) {
                                        selfGraph.nodes --= privateNodes; true
                                      } else false
  }
  @inline final override protected def newEdge(innerEdge: E[NodeT]) = new EdgeT(innerEdge)
  type EdgeSetT = EdgeSet
  @SerialVersionUID(7974L)
  class EdgeSet extends super[GraphLike].EdgeSet with super.EdgeSet
  {
    protected[AdjacencyListGraph] var initialized = false
    
    override protected[collection] def initialize(edges: Iterable[E[N]]) {
      if (edges ne null)
        edges foreach (this add Edge(_))
      initialized = true
    }

    override def add(edge: EdgeT) =
      if (nodes add edge) {
        nrEdges += 1
        true
      } else false

    @inline final protected[collection] def addEdge(edge: EdgeT) { add(edge) }

    def upsert(edge: EdgeT): Boolean = {
      if (nodes upsert edge) {
        nrEdges += 1
        true
      } else false
    }

    override def remove(edge: EdgeT) = 
      if (nodes remove edge) {
        nrEdges -= 1
        true
      } else false

    def removeWithNodes(edge: EdgeT) = {
      val privateNodes = edge.privateNodes
      if (remove(edge)) {
        nodes --= privateNodes
        true
      } else false

    }
    private var nrEdges = 0
    override def size = nrEdges

    @inline final override def maxArity: Int = super.maxArity
  }
  override val edges: EdgeSetT

  @inline final def clear	{	nodes.clear }
  @inline final def add(node: N): Boolean = nodes add Node(node)
  @inline final def add(edge: E[N]): Boolean  = edges add Edge(edge)
  @inline final protected def +=# (edge: E[N]): this.type = { add(edge); this }
  @inline final def upsert(edge: E[N]): Boolean = edges upsert Edge(edge)
}