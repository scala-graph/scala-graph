package scalax.collection
package immutable

import scala.language.higherKinds
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{Set => AnySet}
import scala.collection.mutable.Buffer
import scala.util.Random

import scala.collection.EqSetFacade
import GraphPredef._
import scalax.collection.{Graph => SimpleGraph}
import mutable.{ArraySet, ExtHashSet, EqHashSet, EqHashMap}
import generic.{GroupIterator}
import config.{GraphConfig, AdjacencyListArrayConfig}
import io._

/**
 * Implementation of an incident list based graph representation. This trait is common to
 * both the immutable and mutable variants. An incidence list based representation speeds up
 * traversing the graph along its paths by storing the list of connecting edges at each node.
 *   
 * @author Peter Empen
 */
trait AdjacencyListBase[N,
                        E[X] <: EdgeLikeIn[X],
                       +This[X, Y[X]<:EdgeLikeIn[X]]
                        <: GraphLike[X,Y,This] with AnySet[Param[X,Y]] with SimpleGraph[X,Y]]
  extends GraphLike[N,E,This]
{ selfGraph: This[N,E] =>
  protected type Config <: GraphConfig with AdjacencyListArrayConfig
  
  type NodeT <: InnerNode
  trait InnerNode extends super.InnerNode {
    thisNode: NodeT =>
      
    def edges: ArraySet[EdgeT]
    
    final private def sizeHint(edgesSize: Int): Int =
      if (isHyper) size * 2
      else if (isDirected) if (size < 8) size
                           else (size / 4) * 3
           else size
               
    @inline final protected def nodeEqThis = (n: NodeT) => n eq this
    protected[collection] object Adj extends Serializable { // lazy adjacents
      @transient protected[collection] var _aHook: Option[(NodeT, EdgeT)] = _
      final def aHook: Option[(NodeT, EdgeT)] = {
        if (_aHook eq null) diSucc
        _aHook
      }

      @transient private var _diSucc: EqHashMap[NodeT, EdgeT] = _
      final def diSucc: EqHashMap[NodeT, EdgeT] =
        if (edges eq null)
          new EqHashMap[NodeT, EdgeT]
        else {
          if (_diSucc eq null) {
            val m = new EqHashMap[NodeT, EdgeT](edges.size)
            _aHook = None
            edges foreach { e =>
              if (e.matches(nodeEqThis, nodeEqThis) && aHook.isEmpty)
                _aHook = Some(thisNode -> e)
              addDiSuccessors(e, (n: NodeT) => m put(n, e))
            }
            _diSucc = m
          }
          _diSucc
        }
    }
    import Adj._

    final def connectionsWith(other: NodeT) = edges withSetFilter (_.isAt(other))
    
    final def hasOnlyHooks = diSucc.isEmpty && aHook.isDefined

    final def isDirectPredecessorOf(that: NodeT): Boolean =
      diSucc contains that

    final def isIndependentOf(that: NodeT): Boolean =
      if (this eq that) edges forall (_.nonLooping)
      else              edges forall (! _.isAt((_: NodeT) eq that))

    final def hasSuccessors: Boolean = diSuccessors exists (_ ne this)

    final protected[collection] def addDiSuccessors(edge: EdgeT,
                                                    add: (NodeT) => Unit): Unit = {
      val filter =
        if (edge.isHyperEdge && edge.directed) edge.hasSource((_: NodeT) eq this)
        else true
      edge withTargets (n => if ((n ne this) && filter) add(n))
    }
 
    final def diPredecessors: Set[NodeT] = {
      val m = new EqHashMap[NodeT,EdgeT](edges.size)
      edges foreach { e => addDiPredecessors(e, (n: NodeT) => m put (n, e)) }
      new EqSet(m)
    }

    final def hasPredecessors: Boolean = edges exists (_.hasSource((n: NodeT) => n ne this))

    final protected[collection] def addDiPredecessors(edge: EdgeT,
                                                      add: (NodeT) => Unit) {
      edge withSources (n => if (n ne this) add(n))
    }

    final def neighbors: Set[NodeT] = {
      val m = new EqHashSet[NodeT](edges.size)
      edges foreach { addNeighbors(_, (n: NodeT) => m += n) }
      new EqSetFacade(m)
    }

    final protected[collection] def addNeighbors(edge: EdgeT,
                                                 add: (NodeT) => Unit) {
      edge foreach (n => if (n ne this) add(n))
    }

    final def outgoing = edges withSetFilter (e =>
      if (e.directed) e.hasSource((_: NodeT) eq this)
      else true
    )
    @inline private[this] def isOutgoingTo(e: EdgeT, to: NodeT): Boolean =
      if (e.directed)
        e matches ((_: NodeT) eq this, (_: NodeT) eq to)
      else
        e isAt ((_: NodeT) eq to)

    final def outgoingTo(to: NodeT) = edges withSetFilter (isOutgoingTo(_, to))

    final def findOutgoingTo(to: NodeT): Option[EdgeT] =
      if (to eq this) aHook map (_._2)
      else            diSucc get to

    final def incoming = edges withSetFilter (e =>
      if (e.directed) e.hasTarget((_: NodeT) eq this)
      else true
    )
    @inline final private[this] def isIncomingFrom(e: EdgeT, from: NodeT): Boolean =
      if (e.directed)
        e matches ((_: NodeT) eq from, (_: NodeT) eq this)
      else
        e isAt ((_: NodeT) eq from)

    final def incomingFrom(from: NodeT) =
      edges withSetFilter (isIncomingFrom(_, from))

    final def findIncomingFrom(from: NodeT): Option[EdgeT] =
      edges find (isIncomingFrom(_, from))

    final def degree: Int = (0 /: edges)((cum, e) => cum + e.count(_ eq this) )
    
    final def outDegree: Int = edges count (_.hasSource((n: NodeT) => n eq this))

    final def outDegree(nodeFilter: NodeFilter, edgeFilter: EdgeFilter = anyEdge,
                        includeHooks: Boolean = false, ignoreMultiEdges: Boolean = true): Int = {
      val doEdgeFilter = isCustomEdgeFilter(edgeFilter)
      def edgePred(e: EdgeT): Boolean =
          (if (doEdgeFilter) edgeFilter(e) else true) &&
          e.hasSource((n: NodeT) => n eq this) &&
          e.hasTarget(nodeFilter) &&
          (if (includeHooks) true else ! e.isLooping)
      if (ignoreMultiEdges && isMulti)
        (edges filter edgePred).flatMap(_.targets).toSet.size
      else
        edges count edgePred
    }

    final def inDegree: Int = edges count (_.hasTarget((n: NodeT) => n eq this))

    final def inDegree(nodeFilter: NodeFilter, edgeFilter: EdgeFilter = anyEdge,
                       includeHooks: Boolean = false, ignoreMultiEdges: Boolean = true): Int = {
      val doEdgeFilter = isCustomEdgeFilter(edgeFilter)
      def edgePred(e: EdgeT): Boolean =
          (if (doEdgeFilter) edgeFilter(e) else true) &&
          e.hasTarget((n: NodeT) => n eq this) &&
          e.hasSource(nodeFilter) &&
          (if (includeHooks) true else ! e.isLooping)
      if (ignoreMultiEdges && isMulti)
        (edges filter edgePred).flatMap(_.sources).toSet.size
      else
        edges count edgePred
    }
    
    @inline protected[collection] def add(edge: EdgeT): Boolean = edges add edge

    @inline final protected[collection] def +=(edge: EdgeT): this.type = { add(edge); this }
  }
  @inline final protected def newNode(n: N) = newNodeWithHints(n, config.adjacencyListHints)
  protected def newNodeWithHints(node: N, hints: ArraySet.Hints): NodeT

  type NodeSetT <: NodeSet
  trait NodeSet extends super.NodeSet {
    protected val coll = ExtHashSet.empty[NodeT]
    override protected[collection] def initialize(nodes: Traversable[N],
                                                  edges: Traversable[E[N]]) = {
      if (nodes ne null) 
        coll ++= nodes map (Node(_))
    }
    override protected def copy = {
      val nodeSet = newNodeSet 
      nodeSet.coll ++= this.coll
      nodeSet
    }
    @inline final override def find(elem: N): Option[NodeT] = Option(lookup(elem)) 
    final override def get(outer: N): NodeT = {
      val inner = lookup(outer)
      if (null == inner) throw new NoSuchElementException
      else inner
    }
    final override def lookup(elem: N): NodeT = {
      def eq(inner: NodeT, outer: N) = inner.value == outer
      coll.findEntry[N](elem, eq)
    }
    @inline final def contains(node: NodeT) = coll contains node
    @inline final def iterator: Iterator[NodeT] = coll.iterator
    @inline final override def size: Int = coll.size
    @inline final def draw(random: Random) = coll draw random
    @inline final def findEntry[B](toMatch: B, correspond: (NodeT, B) => Boolean): NodeT =
      coll findEntry (toMatch, correspond)
    protected[collection] def +=(edge: EdgeT): this.type
  }
  def newNodeSet: NodeSetT

  protected def newEdgeTArray(size: Int): Array[EdgeT]

  type EdgeSetT <: EdgeSet
  trait EdgeSet extends super.EdgeSet {
    protected[AdjacencyListBase] def addEdge(edge: EdgeT): Unit
    
    final override def contains(node: NodeT): Boolean = nodes find node exists (_.edges.nonEmpty)
    
    final override def find(elem: E[N] ): Option[EdgeT] = nodes find elem._1 flatMap (_.edges find (_.edge == elem))
    
    final def contains(edge: EdgeT): Boolean = nodes find edge.edge._1 exists (_.edges contains edge)
    
    final def iterator: Iterator[EdgeT] = edgeIterator
    
    private[this] var nrEdges = 0

    private[this] var nrDi = if (selfGraph.isDirectedT) -1 else 0
    final def hasOnlyDiEdges = nrDi == -1 || nrDi == nrEdges

    private[this] var nrHyper = if (selfGraph.isHyperT) 0 else -1
    final def hasAnyHyperEdge = nrHyper > 0

    protected final def statistics(edge: EdgeT, plus: Boolean): Boolean = {
      val add = if (plus) 1 else -1
      nrEdges += add
      if (nrDi    != -1 && edge.isDirected)  nrDi += add
      if (nrHyper != -1 && edge.isHyperEdge) nrHyper += add
      true
    }

    override def size = nrEdges

    def hasAnyMultiEdge = selfGraph.nodes exists { node: NodeT =>
      val (di: Buffer[EdgeT @unchecked], unDi: Buffer[EdgeT  @unchecked]) = {
        if (selfGraph.isDirected) (node.edges.toBuffer[EdgeT], Buffer.empty)
        else node.edges.toBuffer partition (_.isDirected)
      }
      val diTargets, unDiTargets = MSet.empty[NodeT]
      di  .exists((e: EdgeT) => e.hasSource((n: NodeT) => n eq node) && ! e.targets.forall(diTargets add _)) ||
      unDi.exists((e: EdgeT) => (e.n1 eq node)                       && ! e.iterator.drop(1).forall((n: NodeT) => unDiTargets add n))
    }
  }

  def edgeIterator = new GroupIterator[EdgeT] {
    object Outer extends OutermostIterator[NodeT] {
      protected type I = NodeT
      protected val iterator = nodes.iterator
      protected def elmToCurrent(elm: NodeT) = elm

      protected type InnerElm = EdgeT
      protected lazy val inner = Inner
    }
    object Inner extends InnermostIterator[EdgeT] {
      protected type I = EdgeT
      protected var iterator: Iterator[I] = _
      protected def onOuterChange(newOuter: OuterElm) {
        iterator = newOuter.edges.filter(_.edge._1 == newOuter).iterator
      }
      protected def elmToCurrent(elm: EdgeT) = elm

      protected type OuterElm = NodeT
      protected lazy val outer = Outer
    }
    def hasNext = Inner.hasNext
    def next = Inner.next
  }
}