package scalax.collection

import scala.language.{higherKinds, implicitConversions}
import scala.util.Random
import scala.collection.{ExtSetMethods, FilterableSet}

import GraphPredef.{EdgeLikeIn, InnerEdgeParam, InnerNodeParam, OuterEdge}
import GraphEdge.{DiHyperEdgeLike, EdgeLike}
import generic.AnyOrdering

/** Base template trait for graphs.
  *
  * This trait provides the common structure and base operations for immutable graphs
  * independently of their representation. Base operations also cover one-step traversals.
  * For unlimited traversals see `trait GraphTraversal`.
  *
  * Users of Graph usually don't interact directly with this trait but with
  * `trait Graph` instead which inherits the functionality provided by this trait.
  *
  * If `E` inherits `DirectedEdgeLike` the graph is directed, otherwise it is undirected or mixed.
  *
  * @tparam N    the user type of the nodes (vertices) in this graph.
  * @tparam E    the kind of the edges (links) in this graph.
  * @define INNODES The isolated (and optionally any other) outer nodes that the node set of
  *         this graph is to be populated with.
  * @define INEDGES The outer edges that the edge set of this graph is to be populated with.
  *         Nodes being the end of any of these edges will be added to the node set.
  *
  * @author Peter Empen
  */
trait GraphBase[N, E[+X] <: EdgeLikeIn[X]] extends Serializable { selfGraph =>

  /** Populates this graph with `nodes` and `edges`.
    *
    * The implementing class will typically have a constructor with the same parameters
    * which is invoked by `from` of the companion object.
    *
    * @param nodes $INNODES
    * @param edges $INEDGES
    */
  protected def initialize(nodes: Iterable[N], edges: Iterable[E[N]]) {
    this.nodes.initialize(nodes, edges)
    this.edges.initialize(edges)
  }

  /** The order - commonly referred to as |G| - of this graph
    * equaling to the number of nodes.
    */
  def order = nodes.size

  /** `true` if this graph has at most 1 node. */
  @inline final def isTrivial = order <= 1

  /** `true` if this graph has at least 2 nodes. */
  @inline final def nonTrivial = !isTrivial

  /** The size - commonly referred to as ||G|| - of this graph
    * equaling to the number of edges.
    *
    * Method `size` is reserved for the number of nodes and edges
    * because `Graph` is also `SetLike` with set elements being nodes or edges.
    */
  def graphSize = edges.size

  /** Whether all edges of this graph are directed. */
  def isDirected: Boolean

  /** Whether this graph contains at least one hyperedges. */
  def isHyper: Boolean

  /** Whether this graph contains at least one directed and one undirected edge. */
  def isMixed: Boolean

  /** Whether this graph contains at least one multi-edge. We defnie multi-edges by
    *    a. two or more directed edges having the same source and target
    *    a. two or more undirected edges connecting the same nodes
    *    a. two or more (directed) hyperedges that, after being decomposed into (directed) edges,
           yield any multy-edge as stipulated above.
    */
  def isMulti: Boolean

  type NodeFilter = NodeT => Boolean
  type EdgeFilter = EdgeT => Boolean

  // Must be val since eq does not work for def.
  /** Default node filter letting traverse all nodes (non-filter). */
  final val anyNode: NodeFilter = _ => true

  /** Node predicate always returning `false`. */
  final val noNode: NodeFilter = _ => false

  /** Default edge filter letting path all edges (non-filter). */
  final val anyEdge: EdgeFilter = _ => true

  /** `true` if `f` is not equivalent to `anyNode`. */
  @inline final def isCustomNodeFilter(f: NodeFilter) = f ne anyNode

  /** `true` if `f` is not equivalent to `anyEdge`. */
  @inline final def isCustomEdgeFilter(f: EdgeFilter) = f ne anyEdge

  sealed trait InnerElem
  type NodeT <: InnerNode with Serializable
  trait Node extends Serializable
  trait InnerNode extends InnerNodeParam[N] with Node with InnerElem {

    /** The outer node as supplied at instantiation time or while adding nodes this graph.
      * @return Reference to the user-supplied outer node.
      */
    def value: N

    /** Synonym for `value`.
      */
    @inline final def toOuter: N = value

    /** All edges at this node - commonly denoted as E(v).
      * @return all edges connecting to this node.
      */
    def edges: ExtSet[EdgeT]

    /** Synonym for `edges`.
      */
    @inline final def ~ = edges

    /** All edges connecting this node with `other` including outgoing and incoming edges.
      * This method is useful in case of multigraphs.
      *
      * @param other A node which is possibly connected with this node.
      * @return All edges connecting this node with `other`.
      *         If `other` equals this node all hooks are returned.
      *         If `other` is not connected with this node an empty set is returned.
      */
    def connectionsWith(other: NodeT): Set[EdgeT] with FilterableSet[EdgeT]

    /** Checks whether this node has only hooks or no edges at all.
      *  @return `true` if this node has only hooks or it isolated.
      */
    def hasOnlyHooks: Boolean

    /** @return A looping edge out of one or more at this node or `None` if this node has no looping edge. */
    def hook: Option[EdgeT]

    /** Whether `that` is an adjacent (direct successor) to this node.
      *
      * @param that The node to check for adjacency.
      * @return `true` if `that` is adjacent to this node.
      */
    def isDirectPredecessorOf(that: NodeT): Boolean

    /** Whether `that` is independent of this node meaning that
      * there exists no edge connecting this node with `that`.
      *
      * @param that The node to check for independency.
      * @return `true` if `that` node is independent of this node.
      */
    def isIndependentOf(that: NodeT): Boolean

    /** All direct successors of this node, also called ''successor set'' or
      * ''open out-neighborhood'': target nodes of directed incident edges and / or
      * adjacent nodes of undirected incident edges excluding this node.
      *
      * @return set of all direct successors of this node.
      */
    def diSuccessors: Set[NodeT]

    /** Whether this node has any successors. */
    def hasSuccessors: Boolean

    protected[collection] def addDiSuccessors(edge: EdgeT, add: (NodeT) => Unit): Unit

    /** Synonym for `diSuccessors`. */
    @inline final def outNeighbors = diSuccessors

    /** Synonym for `diSuccessors`. */
    @inline final def ~>| = diSuccessors

    /** All direct predecessors of this node, also called ''predecessor set'' or
      * ''open in-neighborhood'': source nodes of directed incident edges and / or
      * adjacent nodes of undirected incident edges excluding this node.
      *
      * @return set of all direct predecessors of this node.
      */
    def diPredecessors: Set[NodeT]

    /** Whether this node has any predecessors. */
    def hasPredecessors: Boolean

    protected[collection] def addDiPredecessors(edge: EdgeT, add: (NodeT) => Unit)

    /** Synonym for `diPredecessors`. */
    @inline final def inNeighbors = diPredecessors

    /** Synonym for `diPredecessors`. */
    @inline final def <~| = diPredecessors

    /** All adjacent nodes (direct successors and predecessors) of this node,
      * also called ''open neighborhood'' excluding this node.
      *
      * @return set of all neighbors.
      */
    def neighbors: Set[NodeT]
    protected[collection] def addNeighbors(edge: EdgeT, add: (NodeT) => Unit)

    /** Synonym for `neighbors`. */
    @inline final def ~| = neighbors

    /** All edges outgoing from this node.
      *
      * @return set of all edges outgoing from this node
      *         including undirected edges and hooks.
      */
    def outgoing: Set[EdgeT] with FilterableSet[EdgeT]

    /** Synonym for `outgoing`. */
    @inline final def ~> = outgoing

    /** All outgoing edges connecting this node with `to`.
      *
      * @param to The node which is the end point of zero, one or more edges starting at this node.
      * @return All edges connecting this node with `to`.
      *         If `to` equals this node all hooks are returned.
      *         If `to` is not an adjacent an empty set is returned.
      */
    def outgoingTo(to: NodeT): Set[EdgeT] with FilterableSet[EdgeT]

    /** Synonym for `outgoingTo`. */
    @inline final def ~>(to: NodeT) = outgoingTo(to)

    /** An outgoing edge connecting this node with `to`.
      *
      * @param to The node which is the end point of an edge starting at this node.
      * @return One of possibly several edges connecting this node with `to`.
      *         If `to` equals this node a hook may be returned.
      *         If `to` is not an adjacent node `None` is returned.
      */
    def findOutgoingTo(to: NodeT): Option[EdgeT]

    /** Synonym for `findOutgoingTo`. */
    @inline final def ~>?(to: NodeT) = findOutgoingTo(to)

    /** Incoming edges of this node.
      *
      * @return set of all edges incoming to of this including undirected edges.
      */
    def incoming: Set[EdgeT] with FilterableSet[EdgeT]

    /** Synonym for `incoming`. */
    @inline final def <~ = incoming

    /** All incoming edges connecting `from` with this node.
      *
      * @param from The node with zero, one or more edges
      *             having this node as a direct successor.
      * @return All edges at `from` having this node as a direct successor.
      *         If `from` equals this node all hooks are returned.
      *         If `from` is not an adjacent node an empty set is returned.
      */
    def incomingFrom(from: NodeT): Set[EdgeT] with FilterableSet[EdgeT]

    /** Synonym for `incomingFrom`. */
    @inline final def <~(from: NodeT) = incomingFrom(from)

    /** An edge at `from` having this node as a successor.
      *
      * @param from The node being at an edge which has
      *           this node as a successor.
      * @return An edges at `from` having this node as a successor.
      *         If `from` equals this node a hook may be returned.
      *         If `from` is not an adjacent node `None` is returned.
      */
    def findIncomingFrom(from: NodeT): Option[EdgeT]

    /** Synonym for `findIncomingFrom`. */
    @inline final def <~?(from: NodeT) = findIncomingFrom(from)

    /** The degree of this node.
      * @return the number of edges that connect to this node. An edge that connects
      *         to this node at more than one ends (loop) is counted as much times as
      *         it is connected to this node. */
    def degree: Int

    /** `true` if this node's degree equals to 0. */
    @inline final def isIsolated = degree == 0

    /** `true` if this node's degree equals to 1. */
    @inline final def isLeaf = degree == 1

    /** The outgoing degree of this node.
      * @return the number of edges that go out from this node including undirected edges.
      *         Loops count once each. */
    def outDegree: Int

    /** The outgoing degree of this node after applying some filters to the outgoing edges and successors.
      */
    def outDegree(nodeFilter: NodeFilter,
                  edgeFilter: EdgeFilter = anyEdge,
                  includeHooks: Boolean = false,
                  ignoreMultiEdges: Boolean = true): Int

    /** The incoming degree of this node.
      * @return the number of edges that come in to this node including undirected edges.
      *         Loops count once each. */
    def inDegree: Int

    /** The incoming degree of this node after applying some filters to the incoming edges and predecessors.
      */
    def inDegree(nodeFilter: NodeFilter,
                 edgeFilter: EdgeFilter = anyEdge,
                 includeHooks: Boolean = false,
                 ignoreMultiEdges: Boolean = true): Int

    def canEqual(that: Any) = true

    override def equals(other: Any) = other match {
      case that: GraphBase[N, E]#InnerNode =>
        (this eq that) || (that canEqual this) && (this.value == that.value)
      case thatR: AnyRef =>
        val thisN = this.value.asInstanceOf[AnyRef]
        (thisN eq thatR) || (thisN == thatR)
      case thatV => this.value == thatV
    }

    override def hashCode = value.##
  }
  object InnerNode {
    def unapply(node: InnerNode): Option[NodeT] =
      if (node isContaining selfGraph) Some(node.asInstanceOf[NodeT])
      else None
  }
  @transient object Node {
    def apply(node: N)    = newNode(node)
    def unapply(n: NodeT) = Some(n)

    @inline final protected[collection] def addDiSuccessors(node: NodeT, edge: EdgeT, add: (NodeT) => Unit) {
      node.addDiSuccessors(edge, add)
    }
    @inline final protected[collection] def addDiPredecessors(node: NodeT, edge: EdgeT, add: (NodeT) => Unit) {
      node.addDiPredecessors(edge, add)
    }
    @inline final protected[collection] def addNeighbors(node: NodeT, edge: EdgeT, add: (NodeT) => Unit) {
      node.addNeighbors(edge, add)
    }

    /** Allows to call methods of N directly on Node instances.
      *
      * @param node
      * @return the contained user Object
      */
    @inline implicit final def toValue[N](node: NodeT) = node.value
  }
  abstract protected class NodeBase extends InnerNode
  protected def newNode(n: N): NodeT

  /** Base trait for graph `Ordering`s. */
  sealed protected trait ElemOrdering {
    protected def noneCompare: Int = throw new IllegalArgumentException("Unexpected use of None.")
    def isDefined: Boolean         = true
  }

  /** The empty ElemOrdering. */
  object NoOrdering extends ElemOrdering with Serializable

  /** Ordering for the path dependent type NodeT. */
  sealed trait NodeOrdering extends Ordering[NodeT] with ElemOrdering
  object NodeOrdering {

    /** Creates a new NodeOrdering with `compare` calling the supplied `cmp`. */
    def apply(cmp: (NodeT, NodeT) => Int) = new NodeOrdering {
      def compare(a: NodeT, b: NodeT): Int = cmp(a, b)
    }
    object None extends NodeOrdering {
      def compare(a: NodeT, b: NodeT): Int = noneCompare
      override def isDefined               = false
    }
  }
  sealed trait EdgeOrdering extends Ordering[EdgeT] with ElemOrdering

  /** Ordering for the path dependent type EdgeT. */
  object EdgeOrdering extends Serializable {

    /** Creates a new EdgeOrdering with `compare` calling the supplied `cmp`. */
    def apply(cmp: (EdgeT, EdgeT) => Int) = new EdgeOrdering {
      def compare(a: EdgeT, b: EdgeT): Int = cmp(a, b)
    }
    object None extends EdgeOrdering {
      def compare(a: EdgeT, b: EdgeT): Int = noneCompare
      override def isDefined               = false
    }
  }

  final protected lazy val anyOrdering = new AnyOrdering[N]
  final lazy val defaultNodeOrdering = NodeOrdering(
    (a: NodeT, b: NodeT) => anyOrdering.compare(a.value, b.value)
  )
  type NodeSetT <: NodeSet
  trait NodeSet extends AnySet[NodeT] with ExtSetMethods[NodeT] {

    /** This method is called by the primary constructor. It must be defined by the trait
      * responsible for the implementation of the graph representation.
      *
      * @param nodes $INNODES
      * @param edges $INEDGES
      */
    protected[collection] def initialize(nodes: Iterable[N], edges: Iterable[E[N]]): Unit
    override def stringPrefix: String = "NodeSet"

    /** Sorts all nodes according to `ord` and concatenates them using `separator`.
      *
      * @param separator to separate nodes by.
      * @param ord custom ordering.
      * @return sorted and concatenated string representation of this node set.
      */
    def asSortedString(separator: String = GraphBase.defaultSeparator)(
        implicit ord: NodeOrdering = defaultNodeOrdering) =
      toList.sorted(ord) mkString separator

    /** Sorts all nodes according to `ord`, concatenates them using `separator`
      * and prefixes and parenthesizes the result with `stringPrefix`.
      *
      * @param separator to separate nodes by.
      * @param ord custom ordering.
      * @return sorted, concatenated and prefixed string representation of this node set.
      */
    def toSortedString(separator: String = GraphBase.defaultSeparator)(
        implicit ord: NodeOrdering = defaultNodeOrdering) =
      stringPrefix + "(" + asSortedString(separator)(ord) + ")"

    /** Converts this node set to a set of outer nodes.
      */
    def toOuter: Set[N] = {
      val b = Set.newBuilder[N]
      this foreach (b += _)
      b.result
    }
    @deprecated("Use toOuter instead", "1.8.0") def toNodeInSet = toOuter

    /** Finds the inner node corresponding to `outerNode`.
      *
      * @return the inner node wrapped by `Some` if found, otherwise None.
      */
    def find(outerNode: N): Option[NodeT]

    /** Finds the inner node corresponding to `outerNode`.
      *
      * @return the inner node if found, otherwise `NoSuchElementException` is thrown.
      */
    def get(outerNode: N): NodeT

    /** Finds the inner node corresponding to `outerNode`.
      *
      * @return the inner node if found, otherwise `null`.
      */
    def lookup(outerNode: N): NodeT

    def adjacencyListsToString: String =
      (for (n <- this)
        yield n.value.toString + ": " + ((for (a <- n.diSuccessors) yield a.value) mkString ",")) mkString "\n"

    def draw(random: Random): NodeT

    final override def diff(that: AnySet[NodeT]): NodeSetT = ??? //(this -- that).asInstanceOf[NodeSetT]
  }

  /** The node (vertex) set of this `Graph` commonly referred to as V(G).
    *
    * @return Set of all contained nodes.
    */
  def nodes: NodeSetT

  type EdgeT <: InnerEdgeParam[N, E, NodeT, E] with InnerEdge with Serializable
  @transient object EdgeT {
    def unapply(e: EdgeT): Option[(NodeT, NodeT)] = Some((e.edge._1, e.edge._2))
  }

  trait Edge extends Serializable
  trait InnerEdge extends Iterable[NodeT] with InnerEdgeParam[N, E, NodeT, E] with Edge with InnerElem {
    this: EdgeT =>

    override def stringPrefix = super[InnerEdgeParam].stringPrefix

    /** The outer edge after transformation by means of the `copy` method.
      * This edge contains references to inner nodes while the original outer
      * edge contained references to outer nodes.
      */
    def edge: E[NodeT]

    /** The inner nodes incident with this inner edge.
      * This is just a synonym to `this` that extends `Iterable[NodeT]`.
      */
    @inline final def nodes: Iterable[NodeT] = this

    /** Finds nodes of this edge which only participate in this edge.
      *
      * @return those nodes of this edge which do not participate in any other edge
      */
    def privateNodes: Set[NodeT] = filter(_.edges.size == 1).toSet

    /** All connecting edges, that is all edges at any of the nodes incident with this edge.
      *
      * @return set of connecting edges including hooks.
      */
    def adjacents: Set[EdgeT] = {
      val a = new mutable.EqHashMap[EdgeT, Null]
      this foreach (n => n.edges foreach (e => a put (e, null)))
      a -= this
      new immutable.EqSet(a)
    }

    /** Synonym for `adjacents`. */
    @inline final def ~~ = adjacents

    /** The head (target node) of a directed edge or `_2` otherwise.
      */
    /* With 2.10, 'edge.to' can no more be called implicitly because of the addition of
     * 'to' to GenTraversableOnce in the standard library. So we must delegate the call. */
    def to: NodeT = edge match {
      case di: DiHyperEdgeLike[NodeT] => di.to
      case unDi                       => unDi.edge._2
    }

    // TODO do we want to keep this? Iterable no longer extends trait Equals (previously through IterableLike)
    /*override*/ def canEqual(that: Any) = that.isInstanceOf[GraphBase[N, E]#InnerEdge] ||
      that.isInstanceOf[EdgeLike[_]]
    override def equals(other: Any) = other match {
      case that: GraphBase[N, E]#InnerEdge =>
        (this eq that) ||
          (this.edge eq that.edge) ||
          (this.edge == that.edge)
      case that: EdgeLike[_] =>
        (this.edge eq that) ||
          (this.edge == that)
      case _ => false
    }
    override def hashCode = edge.##
    override def toString = edge.toString

    /** Reconstructs the outer edge by means of the `copy` method. */
    def toOuter: E[N] = {
      val newNs = (edge.arity: @scala.annotation.switch) match {
        case 2 => Tuple2(edge._1.value, edge._2.value)
        case 3 => Tuple3(edge._1.value, edge._2.value, edge._n(2).value)
        case 4 => Tuple4(edge._1.value, edge._2.value, edge._n(2).value, edge._n(3).value)
        case 5 => Tuple5(edge._1.value, edge._2.value, edge._n(2).value, edge._n(3).value, edge._n(4).value)
        case _ => ??? //edge.map(n => n.value).toList
      }
      edge.copy[N](newNs).asInstanceOf[E[N]]
    }
    @deprecated("Use toOuter instead", "1.8.0") def toEdgeIn = toOuter
  }

  @transient object InnerEdge {
    def unapply(edge: InnerEdge): Option[EdgeT] =
      if (edge.edge._1 isContaining selfGraph) Some(edge.asInstanceOf[EdgeT])
      else None
  }
  @transient object Edge {
    def apply(innerEdge: E[NodeT]) = newEdge(innerEdge)
    def unapply(e: EdgeT)          = Some(e)

    private[this] var freshNodes: Map[N, NodeT] = _
    private def mkNode(n: N): NodeT = {
      val existing = nodes lookup n
      if (null eq existing)
        freshNodes getOrElse (n, {
          val newN = newNode(n)
          freshNodes += (n -> newN)
          newN
        })
      else existing
    }

    /** Creates a new inner edge from the outer `edge` using its
      *  factory method `copy` without modifying the node or edge set. */
    protected[GraphBase] def edgeToEdgeCont(edge: E[N]): E[NodeT] = {
      freshNodes = Map.empty[N, NodeT]
      val newNodes = (edge.arity: @scala.annotation.switch) match {
        case 2 => Tuple2(mkNode(edge._1), mkNode(edge._2))
        case 3 => Tuple3(mkNode(edge._1), mkNode(edge._2), mkNode(edge._n(2)))
        case 4 => Tuple4(mkNode(edge._1), mkNode(edge._2), mkNode(edge._n(2)), mkNode(edge._n(3)))
        case 5 => Tuple5(mkNode(edge._1), mkNode(edge._2), mkNode(edge._n(2)), mkNode(edge._n(3)), mkNode(edge._n(4)))
        case _ => ??? //(edge map mkNode).toList
      }
      edge.copy[NodeT](newNodes).asInstanceOf[E[NodeT]]
    }
    def mkNodes(node_1: N, node_2: N, nodes: N*): Product = {
      freshNodes = Map.empty[N, NodeT]
      val (n_1, n_2) = (mkNode(node_1), mkNode(node_2))
      val newNodes =
        if (nodes.isEmpty) Tuple2(n_1, n_2)
        else
          (nodes.size: @scala.annotation.switch) match {
            case 1 => Tuple3(n_1, n_2, mkNode(nodes(0)))
            case 2 => Tuple4(n_1, n_2, mkNode(nodes(0)), mkNode(nodes(1)))
            case 3 => Tuple5(n_1, n_2, mkNode(nodes(0)), mkNode(nodes(1)), mkNode(nodes(2)))
            case _ => ??? //(nodes map mkNode).toList ::: List(n_1, n_2)
          }
      newNodes
    }
    implicit final def innerEdgeToEdgeCont(edge: EdgeT): E[NodeT] = edge.edge

    def defaultWeight(edge: EdgeT)                                       = edge.weight
    def weightOrdering[T: Numeric](weightF: EdgeT => T): Ordering[EdgeT] = Ordering by weightF
    def weightOrdering(weightF: EdgeT => Double): Ordering[EdgeT]        = Ordering by weightF

    object WeightOrdering extends Ordering[EdgeT] {
      def compare(e1: EdgeT, e2: EdgeT): Int = e1.weight compare e2.weight
    }
    object ArityOrdering extends Ordering[EdgeT] {
      def compare(e1: EdgeT, e2: EdgeT): Int = e1.arity compare e2.arity
    }
  }

  protected def newEdge(innerEdge: E[NodeT]): EdgeT
  implicit final protected def edgeToEdgeCont(e: E[N]): E[NodeT] = Edge.edgeToEdgeCont(e)

  final lazy val defaultEdgeOrdering = EdgeOrdering(
    (a: EdgeT, b: EdgeT) => {
      val unequal: Option[(NodeT, NodeT)] = (a.edge zip b.edge) find (z => z._1 != z._2)
      unequal map (t => anyOrdering.compare(t._1.value, t._2.value)) getOrElse
        Ordering.Int.compare(a.arity, b.arity)
    }
  )

  type EdgeSetT <: EdgeSet
  trait EdgeSet extends AnySet[EdgeT] with ExtSetMethods[EdgeT] with Serializable {

    /** This method is called by the primary constructor. It must be defined by the trait
      * responsible for the implementation of the graph representation.
      *
      * @param edges $INEDGES
      */
    protected[collection] def initialize(edges: Iterable[E[N]]): Unit
    def contains(node: NodeT): Boolean
    override def stringPrefix: String = "EdgeSet"

    /** Sorts all edges according to `ord` and concatenates them using `separator`.
      *
      * @param separator to separate edges by.
      * @param ord custom ordering.
      * @return sorted and concatenated string representation of this edge set.
      */
    def asSortedString(separator: String = GraphBase.defaultSeparator)(
        implicit ord: EdgeOrdering = defaultEdgeOrdering) =
      toList.sorted(ord) mkString separator

    /** Sorts all edges according to `ord`, concatenates them using `separator`
      * and prefixes and parenthesizes the result with `stringPrefix`.
      *
      * @param separator to separate edges by.
      * @param ord custom ordering.
      * @return sorted, concatenated and prefixed string representation of this edge set.
      */
    def toSortedString(separator: String = GraphBase.defaultSeparator)(
        implicit ord: EdgeOrdering = defaultEdgeOrdering) =
      stringPrefix + "(" + asSortedString(separator)(ord) + ")"

    /** Finds the inner edge corresponding to `outerEdge`.
      *
      * @return the inner node wrapped by `Some` if found, otherwise None.
      */
    def find(outerEdge: E[N]): Option[EdgeT]

    /** The maximum arity of all edges in this edge set. */
    def maxArity: Int = if (size == 0) 0 else max(Edge.ArityOrdering).arity

    /** Converts this edge set to a set of outer edges.
      */
    def toOuter: Set[E[N]] = {
      val b = Set.newBuilder[E[N]]
      this foreach (b += _.toOuter)
      b.result
    }
    @deprecated("Use toOuter instead", "1.8.0") def toEdgeInSet = toOuter

    final def draw(random: Random) = (nodes draw random).edges draw random
    final def findElem[B](other: B, correspond: (EdgeT, B) => Boolean): EdgeT = {
      def find(edge: E[N]): EdgeT = correspond match {
        case c: ((EdgeT, E[N]) => Boolean) @unchecked => nodes.lookup(edge._1).edges findElem (edge, c)
        case _                                        => throw new IllegalArgumentException
      }
      other match {
        case e: OuterEdge[N, E]            => find(e.edge)
        case e: InnerEdgeParam[N, E, _, E] => find(e.asEdgeT[N, E, selfGraph.type](selfGraph).toOuter)
        case _                                => null.asInstanceOf[EdgeT]
      }
    }

    final override def diff(that: AnySet[EdgeT]): EdgeSetT = ??? // (this -- that).asInstanceOf[EdgeSetT]
  }

  /** The edge set of this `Graph` commonly referred to as E(G).
    *
    * @return Set of all contained edges.
    */
  def edges: EdgeSetT
  def totalWeight = (0d /: edges)(_ + _.weight)
}

object GraphBase {
  val defaultSeparator = ", "
}
