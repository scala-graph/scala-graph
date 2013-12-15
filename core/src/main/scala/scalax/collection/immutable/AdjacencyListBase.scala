package scalax.collection
package immutable

import language.higherKinds
import scala.util.Random
import scala.annotation.unchecked.uncheckedVariance

import GraphPredef._
import scalax.collection.{Graph => SimpleGraph}
import mutable.{ArraySet, ExtHashSet}
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
                        <: GraphLike[X,Y,This] with Set[GraphParam[X,Y]] with SimpleGraph[X,Y]]
  extends GraphLike[N,E,This]
  with    GraphAux [N,E]
{ this: This[N,E] =>
  protected type Config <: GraphConfig with AdjacencyListArrayConfig
  
  @inline final protected
  def from(that: AdjacencyListBase[N,E,This @uncheckedVariance])
          (delNodes: Iterable[AdjacencyListBase[N,E,This @uncheckedVariance]#NodeT] = Seq(),
           delEdges: Iterable[AdjacencyListBase[N,E,This @uncheckedVariance]#EdgeT] = Seq(),
           ripple:   Boolean              = false,
           addNodes: Iterable[N]          = Seq(),
           addEdges: Iterable[E[N]]       = Seq()) {
    nodes.from(that)(delNodes, delEdges, ripple, addNodes, addEdges)
  }

  type NodeT <: InnerNodeLike
  trait InnerNodeLike extends super.InnerNodeLike {
    this: NodeT =>
    def edges: ArraySet[EdgeT]
    @inline final protected[collection] def +=(edge: EdgeT): this.type = {
      edges add edge; this
    }
  }
  @inline final protected def newNode(n: N) = newNodeWithHints(n, config.adjacencyListHints)
  protected def newNodeWithHints(node: N, hints: ArraySet.Hints): NodeT

  type NodeSetT <: NodeSet
  trait NodeSet extends NodeSetAux with super.NodeSet {
    protected val coll = ExtHashSet.empty[NodeT]
    override protected[collection] def initialize(nodes: collection.Iterable[N],
                                                  edges: collection.Iterable[E[N]]) = {
      if (nodes ne null) 
        coll ++= nodes map (Node(_))
    }
    override protected[collection] def from (
        nodeStreams: Iterable[NodeInputStream[N]],
        nodes:       Iterable[N],
        edgeStreams: Iterable[GenEdgeInputStream[N,E]],
        edges:       Iterable[E[N]]) {
      for (n <- new NodeAux.NodeContStream(nodeStreams, nodes))
        coll += Node(n)
    }
    override protected def copy = {
      val nodeSet = newNodeSet 
      nodeSet.coll ++= this.coll
      nodeSet
    }
    protected[AdjacencyListBase]
    def from(that: AdjacencyListBase[N,E,This @uncheckedVariance])
            (delNodes: Iterable[AdjacencyListBase[N,E,This @uncheckedVariance]#NodeT],
             delEdges: Iterable[AdjacencyListBase[N,E,This @uncheckedVariance]#EdgeT],
             ripple:   Boolean,
             addNodes: Iterable[N],
             addEdges: Iterable[E[N]]) {
      val thatNodes = that.nodes 
      val newEdges = newEdgeTArray(that.graphSize)
      var cnt = 0

      val skipNodes = delNodes.toSet
      val skipEdges = delEdges.toSet
      /* clone all nodes except those contained in delNodes;
       * doing so add edges with their first node being this node to the incident set
       */
      for (thatNode <- thatNodes if ! skipNodes(thatNode)) {
        @inline def eqNodes(thisNode: NodeT, thatNode: AdjacencyListBase[N,E,This]#NodeT) = 
          thisNode.value == thatNode.value
        def get(thatNode: AdjacencyListBase[N,E,This]#NodeT): NodeT = {
          val thisNode = coll findEntry (thatNode, eqNodes)
          if (null == thisNode) {
            val n = newNodeWithHints(thatNode.value, thatNode.edges.hints)
            coll += n; n
          } else thisNode
        }
        val thisNode = get(thatNode)
        def include(edge: that.EdgeT) =
          ! skipEdges(edge) &&
          (if (edge.arity == 2) ! skipNodes(edge.edge._2)
           else edge.nodes.tail forall (! skipNodes(_)))

        for (thatEdge <- thatNode.edges if thatEdge.edge._1 == thisNode &&
                                           include(thatEdge)) {
          val e = thatEdge.edge
          val thisEdge = newEdge(e.copy[NodeT]((e.arity: @scala.annotation.switch) match {
            case 2 => Tuple2(thisNode, get(e._2))
            case 3 => Tuple3(thisNode, get(e._2), get(e._n(2)))
            case 4 => Tuple4(thisNode, get(e._2), get(e._n(2)), get(e._n(3)))
            case 5 => Tuple5(thisNode, get(e._2), get(e._n(2)), get(e._n(3)), get(e._n(4)))
            case _ => e.map(n => get(n)).toList 
          }).asInstanceOf[E[NodeT]])

          thisNode.edges +=! thisEdge
          newEdges(cnt) = thisEdge; cnt += 1
        }
      }
      /* insert edges from the newEdges buffer at nodes
       * being any of the edge's non-first incident nodes 
       */
      val nrEdges = cnt
      cnt = 0
      while (cnt < nrEdges) {
        val newEdge = newEdges(cnt)
        for (node <- newEdge.nodes.tail if node ne newEdge.edge._1)
          node += newEdge
        cnt += 1
      }
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
    @inline final def draw(random: Random) = coll draw random
    @inline final def findEntry[B](toMatch: B, correspond: (NodeT, B) => Boolean): NodeT =
      coll findEntry (toMatch, correspond)
    protected[collection] def +=(edge: EdgeT): this.type
  }
  def newNodeSet: NodeSetT

  protected def newEdgeTArray(size: Int): Array[EdgeT]

  type EdgeSetT <: EdgeSet
  trait EdgeSet extends super.EdgeSet with EdgeSetAux {
    override protected[collection] def from(
        nodeStreams: Iterable[NodeInputStream[N]],
        nodes:       Iterable[N],
        edgeStreams: Iterable[GenEdgeInputStream[N,E]],
        edges:       Iterable[E[N]])
    {
      for (e <- new EdgeAux.EdgeContStream(edgeStreams, edges))
        addEdge(Edge(e))
    }
    protected[AdjacencyListBase] def addEdge(edge: EdgeT): Unit
    final override def contains(node: NodeT): Boolean =
      nodes find node exists (_.edges.nonEmpty)
    final override def find(elem: E[N] ): Option[EdgeT] =
      nodes find (elem._1) flatMap (_.edges find (_.edge == elem))
    final def contains(edge: EdgeT): Boolean =
      nodes find (edge.edge._1) exists (_.edges contains edge)
    final def iterator: Iterator[EdgeT] = edgeIterator
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