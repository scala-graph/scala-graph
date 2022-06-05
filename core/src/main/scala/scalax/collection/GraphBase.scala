package scalax.collection

import scala.language.implicitConversions
import scala.util.Random
import scala.collection.{ExtSetMethods, FilterableSet}

import GraphEdge._
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
  * @author Peter Empen
  */
trait GraphBase[N, E <: EdgeLike[N], +This[X, Y <: EdgeLike[X]] <: GraphBase[X, Y, This]]
    extends GraphOps[N, E, This]
    with OuterElems[N, E]
    with Serializable { selfGraph =>

  /** Populates this graph with `nodes` and `edges`.
    * The implementing class will typically have a constructor with the same parameters
    * which is invoked by `from` of the companion object.
    * @param nodes $INNODES
    * @param edges $INEDGES
    */
  protected def initialize(nodes: Iterable[N], edges: Iterable[E]): Unit = {
    this.nodes.initialize(nodes, edges)
    this.edges.initialize(edges)
  }

  /** The order - commonly referred to as |G| - of this graph equaling to the number of nodes. */
  final def order: Int = nodes.size

  /** The size - commonly referred to as |E| - of this graph equaling to the number of edges. */
  final def size: Int = edges.size

  // The following predicates must be val because eq does not work for def.
  /** Default node filter letting traverse all nodes (non-filter). */
  final val anyNode: NodePredicate = _ => true

  /** Node predicate always returning `false`. */
  final val noNode: NodePredicate = _ => false

  /** Default edge filter letting path all edges (non-filter). */
  final val anyEdge: EdgePredicate = _ => true

  /** Edge predicate always returning `false`. */
  final val noEdge: EdgePredicate = _ => false

  /** `true` if `f` is not equivalent to `anyNode`. */
  @inline final def isCustomNodeFilter(f: NodePredicate) = f ne anyNode

  /** `true` if `f` is not equivalent to `anyEdge`. */
  @inline final def isCustomEdgeFilter(f: EdgePredicate) = f ne anyEdge

  type NodeT <: BaseInnerNode with Serializable
  trait Node          extends Serializable
  trait BaseInnerNode extends Node with InnerNode {

    /** All edges at this node - commonly denoted as E(v).
      * @return all edges connecting to this node.
      */
    def edges: ExtSet[EdgeT]

    /** Synonym for `edges`. */
    @inline final def ~ : ExtSet[EdgeT] = edges

    /** All edges connecting this node with `other` including outgoing and incoming edges.
      * This method is useful in case of multigraphs.
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
      * @param that The node to check for adjacency.
      * @return `true` if `that` is adjacent to this node.
      */
    def isDirectPredecessorOf(that: NodeT): Boolean

    /** Whether `that` is independent of this node meaning that
      * there exists no edge connecting this node with `that`.
      * @param that The node to check for independency.
      * @return `true` if `that` node is independent of this node.
      */
    def isIndependentOf(that: NodeT): Boolean

    /** All direct successors of this node, also called ''successor set'' or
      * ''open out-neighborhood'': target nodes of directed incident edges and / or
      * adjacent nodes of undirected incident edges excluding this node.
      * @return set of all direct successors of this node.
      */
    def diSuccessors: Set[NodeT]

    /** Whether this node has any successors. */
    def hasSuccessors: Boolean

    protected[collection] def addDiSuccessors(edge: EdgeT, add: NodeT => Unit): Unit

    /** Synonym for `diSuccessors`. */
    @inline final def outNeighbors: Set[NodeT] = diSuccessors

    /** Synonym for `diSuccessors`. */
    @inline final def ~>| : Set[NodeT] = diSuccessors

    /** All direct predecessors of this node, also called ''predecessor set'' or
      * ''open in-neighborhood'': source nodes of directed incident edges and / or
      * adjacent nodes of undirected incident edges excluding this node.
      * @return set of all direct predecessors of this node.
      */
    def diPredecessors: Set[NodeT]

    /** Whether this node has any predecessors. */
    def hasPredecessors: Boolean

    protected[collection] def addDiPredecessors(edge: EdgeT, add: NodeT => Unit): Unit

    /** Synonym for `diPredecessors`. */
    @inline final def inNeighbors = diPredecessors

    /** Synonym for `diPredecessors`. */
    @inline final def <~| = diPredecessors

    /** All adjacent nodes (direct successors and predecessors) of this node,
      * also called ''open neighborhood'' excluding this node.
      * @return set of all neighbors.
      */
    def neighbors: Set[NodeT]
    protected[collection] def addNeighbors(edge: EdgeT, add: NodeT => Unit): Unit

    /** Synonym for `neighbors`. */
    @inline final def ~| : Set[NodeT] = neighbors

    /** All edges outgoing from this node.
      * @return set of all edges outgoing from this node
      *         including undirected edges and hooks.
      */
    def outgoing: Set[EdgeT] with FilterableSet[EdgeT]

    /** Synonym for `outgoing`. */
    @inline final def ~> : Set[EdgeT] with FilterableSet[EdgeT] = outgoing

    /** All outgoing edges connecting this node with `to`.
      * @param to The node which is the end point of zero, one or more edges starting at this node.
      * @return All edges connecting this node with `to`.
      *         If `to` equals this node all hooks are returned.
      *         If `to` is not an adjacent an empty set is returned.
      */
    def outgoingTo(to: NodeT): Set[EdgeT] with FilterableSet[EdgeT]

    /** Synonym for `outgoingTo`. */
    @inline final def ~>(to: NodeT) = outgoingTo(to)

    /** An outgoing edge connecting this node with `to`.
      * @param to The node which is the end point of an edge starting at this node.
      * @return One of possibly several edges connecting this node with `to`.
      *         If `to` equals this node a hook may be returned.
      *         If `to` is not an adjacent node `None` is returned.
      */
    def findOutgoingTo(to: NodeT): Option[EdgeT]

    /** Synonym for `findOutgoingTo`. */
    @inline final def ~>?(to: NodeT) = findOutgoingTo(to)

    /** Incoming edges of this node.
      * @return set of all edges incoming to of this including undirected edges.
      */
    def incoming: Set[EdgeT] with FilterableSet[EdgeT]

    /** Synonym for `incoming`. */
    @inline final def <~ = incoming

    /** All incoming edges connecting `from` with this node.
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
      *         it is connected to this node.
      */
    def degree: Int

    /** `true` if this node's degree equals to 0. */
    @inline final def isIsolated = degree == 0

    /** `true` if this node's degree equals to 1. */
    @inline final def isLeaf = degree == 1

    /** The outgoing degree of this node.
      * @return the number of edges that go out from this node including undirected edges.
      *         Loops count once each.
      */
    def outDegree: Int

    /** The outgoing degree of this node after applying some filters to the outgoing edges and successors.
      */
    def outDegree(
        nodeFilter: NodePredicate,
        edgeFilter: EdgePredicate = anyEdge,
        includeHooks: Boolean = false,
        ignoreMultiEdges: Boolean = true
    ): Int

    /** The incoming degree of this node.
      * @return the number of edges that come in to this node including undirected edges.
      *         Loops count once each.
      */
    def inDegree: Int

    /** The incoming degree of this node after applying some filters to the incoming edges and predecessors. */
    def inDegree(
        nodeFilter: NodePredicate,
        edgeFilter: EdgePredicate = anyEdge,
        includeHooks: Boolean = false,
        ignoreMultiEdges: Boolean = true
    ): Int

    def canEqual(that: Any) = true

    override def equals(other: Any) = other match {
      case that: GraphBase[N, E, This]#BaseInnerNode =>
        (this eq that) || (that canEqual this) && this.outer == that.outer
      case thatR: AnyRef =>
        val thisN = this.outer.asInstanceOf[AnyRef]
        (thisN eq thatR) || thisN == thatR
      case thatV => this.outer == thatV
    }

    override def hashCode: Int    = outer.##
    override def toString: String = outer.toString
  }

  @transient object Node {
    def apply(node: N)    = newNode(node)
    def unapply(n: NodeT) = Some(n)

    @inline final protected[collection] def addDiSuccessors(node: NodeT, edge: EdgeT, add: NodeT => Unit): Unit =
      node.addDiSuccessors(edge, add)
    @inline final protected[collection] def addDiPredecessors(node: NodeT, edge: EdgeT, add: NodeT => Unit): Unit =
      node.addDiPredecessors(edge, add)
    @inline final protected[collection] def addNeighbors(node: NodeT, edge: EdgeT, add: NodeT => Unit): Unit =
      node.addNeighbors(edge, add)

    /** Allows to call methods of N directly on Node instances. */
    @inline implicit final def toOuter(node: NodeT): N = node.outer
  }
  abstract protected class BaseNodeBase extends BaseInnerNode
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

  /** Ordering for the path dependent type EdgeT. */
  sealed trait EdgeOrdering extends Ordering[EdgeT] with ElemOrdering

  /** Ordering for the path dependent type EdgeT. */
  object EdgeOrdering extends Serializable {

    def apply(cmp: (EdgeT, EdgeT) => Int): EdgeOrdering = new EdgeOrdering {
      def compare(a: EdgeT, b: EdgeT): Int = cmp(a, b)
    }

    def apply(weight: EdgeT => Double): EdgeOrdering = new EdgeOrdering {
      def compare(x: EdgeT, y: EdgeT): Int = Ordering[Double].compare(weight(x), weight(y))
    }

    object None extends EdgeOrdering {
      def compare(a: EdgeT, b: EdgeT): Int = noneCompare
      override def isDefined               = false
    }
  }

  final protected lazy val anyOrdering = new AnyOrdering[N]
  final lazy val defaultNodeOrdering   = NodeOrdering((a: NodeT, b: NodeT) => anyOrdering.compare(a.outer, b.outer))
  type NodeSetT <: NodeSet
  trait NodeSet extends AnySet[NodeT] with ExtSetMethods[NodeT] {

    /** This method is called by the primary constructor. It must be defined by the trait
      * responsible for the implementation of the graph representation.
      *
      * @param nodes $INNODES
      * @param edges $INEDGES
      */
    protected[collection] def initialize(nodes: Iterable[N], edges: Iterable[E]): Unit
    final override protected def className: String = "NodeSet"

    /** Sorts all nodes according to `ord` and concatenates them using `separator`.
      *
      * @param separator to separate nodes by.
      * @param ord custom ordering.
      * @return sorted and concatenated string representation of this node set.
      */
    def asSortedString(separator: String = GraphBase.defaultSeparator)(implicit
        ord: NodeOrdering = defaultNodeOrdering
    ): String =
      toList.sorted(ord) mkString separator

    /** Sorts all nodes according to `ord`, concatenates them using `separator`
      * and prefixes and parenthesizes the result with `stringPrefix`.
      *
      * @param separator to separate nodes by.
      * @param ord custom ordering.
      * @return sorted, concatenated and prefixed string representation of this node set.
      */
    def toSortedString(separator: String = GraphBase.defaultSeparator)(implicit
        ord: NodeOrdering = defaultNodeOrdering
    ): String =
      stringPrefix + "(" + asSortedString(separator)(ord) + ")"

    /** Iterator over this `NodeSet` mapped to outer nodes. */
    @inline final def outerIterator: Iterator[N] = iterator map (_.outer)

    /** `Iterable` over this `NodeSet` mapped to outer nodes. */
    @inline final def outerIterable: Iterable[N] = map(_.outer)

    /** Converts this node set to a set of outer nodes. */
    def toOuter: Set[N] = {
      val b = Set.newBuilder[N]
      this foreach (b += _)
      b.result()
    }

    /** Finds the inner node corresponding to `outerNode`.
      * @return the inner node wrapped by `Some` if found, otherwise None.
      */
    def find(outerNode: N): Option[NodeT]

    /** Finds the inner node corresponding to `outerNode`.
      * @return the inner node if found, otherwise `NoSuchElementException` is thrown.
      */
    def get(outerNode: N): NodeT

    /** Finds the inner node corresponding to `outerNode`.
      * @return the inner node if found, otherwise `null`.
      */
    def lookup(outerNode: N): NodeT

    def adjacencyListsToString: String =
      (for (n <- this)
        yield n.outer.toString + ": " + ((for (a <- n.diSuccessors) yield a.outer) mkString ",")) mkString "\n"

    def draw(random: Random): NodeT

    def diff(that: AnySet[NodeT]): AnySet[NodeT] = this.toSet diff that
  }

  /** The node (vertex) set of this `Graph` commonly referred to as V(G).
    * @return Set of all contained nodes.
    */
  def nodes: NodeSetT

  type EdgeT <: InnerEdgeLike[NodeT] with BaseInnerEdge
  trait BaseInnerEdge extends InnerEdgeLike[NodeT] with InnerEdge with Equals {
    this: EdgeT =>

    @inline final override def weight: Double = outer.weight

    /** Finds nodes of this edge which only participate in this edge. */
    def privateNodes: Set[NodeT] = ends.filter(_.edges.size == 1).toSet

    /** All connecting edges, that is all edges with ends incident with this edge including possible loops. */
    def adjacents: Set[EdgeT] = {
      val a = new mutable.EqHashMap[EdgeT, Null]
      ends foreach (n => n.edges foreach (e => a put (e, null)))
      a -= this
      new immutable.EqSet(a)
    }

    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[GraphBase[N, E, This]#BaseInnerEdge] ||
        that.isInstanceOf[EdgeLike[_]]

    override def equals(other: Any): Boolean = other match {
      case that: GraphBase[N, E, This]#BaseInnerEdge =>
        (this eq that) ||
        (this.outer eq that.outer) ||
        this.outer == that.outer
      case that: EdgeLike[_] =>
        (this.outer eq that) ||
        this.outer == that
      case _ => false
    }

    override def hashCode: Int    = outer.##
    override def toString: String = outer.toString
  }
  @transient object BaseInnerEdge {
    protected[collection] def apply(outer: E): EdgeT = {
      @inline def lookup(n: N) = nodes lookup n

      val freshNodes = MMap.empty[N, NodeT]

      def inner(ends: Iterable[N]): Iterable[NodeT] = {
        def mkNode(n: N): NodeT =
          Option(lookup(n)) getOrElse (
            freshNodes getOrElse (n, {
              val newN = newNode(n)
              freshNodes += n -> newN
              newN
            })
          )
        ends map mkNode
      }

      outer match {
        case edge: AnyEdge[N] =>
          val (n_1, n_2) = (edge._n(0), edge._n(1))
          @inline def inner(n: N): NodeT = {
            val found = lookup(n)
            if (null eq found) newNode(n) else found
          }
          val inner_1 = inner(n_1)
          val inner_2 = if (n_1 == n_2) inner_1 else inner(n_2)

          newEdge(outer, inner_1, inner_2)
        case diHyper: AnyDiHyperEdge[N] => newDiHyperEdge(outer, inner(diHyper.sources), inner(diHyper.targets))
        case hyper: AnyHyperEdge[N]     => newHyperEdge(outer, inner(hyper.ends))
      }
    }

    def defaultWeight(edge: EdgeT): Double                            = edge.weight
    def weightOrdering[T: Numeric](weightF: EdgeT => T): EdgeOrdering = EdgeOrdering((Ordering by weightF).compare _)
    def weightOrdering(weightF: EdgeT => Double): EdgeOrdering        = EdgeOrdering(weightF)

    object WeightOrdering extends EdgeOrdering {
      def compare(e1: EdgeT, e2: EdgeT): Int = e1.weight compare e2.weight
    }
    object ArityOrdering extends EdgeOrdering {
      def compare(e1: EdgeT, e2: EdgeT): Int = e1.arity compare e2.arity
    }

    /** Allows to call methods of E directly on EdgeT instances. */
    implicit final def toOuterEdge(edge: EdgeT): E = edge.outer
  }

  protected def newHyperEdge(outer: E, nodes: Iterable[NodeT]): EdgeT
  protected def newDiHyperEdge(outer: E, sources: Iterable[NodeT], targets: Iterable[NodeT]): EdgeT
  protected def newEdge(outer: E, node_1: NodeT, node_2: NodeT): EdgeT

  final lazy val defaultEdgeOrdering = EdgeOrdering { (a: EdgeT, b: EdgeT) =>
    (a.ends zip b.ends)
      .find(z => z._1 != z._2)
      .map(t => anyOrdering.compare(t._1, t._2))
      .getOrElse(Ordering.Int.compare(a.arity, b.arity))
  }

  type EdgeSetT <: EdgeSet
  trait EdgeSet extends AnySet[EdgeT] with ExtSetMethods[EdgeT] with Serializable {

    /** This method is called by the primary constructor. It must be defined by the trait
      * responsible for the implementation of the graph representation.
      *
      * @param edges $INEDGES
      */
    protected[collection] def initialize(edges: Iterable[E]): Unit
    def contains(node: NodeT): Boolean

    final override protected def className: String = "EdgeSet"

    /** Sorts all edges according to `ord` and concatenates them using `separator`.
      *
      * @param separator to separate edges by.
      * @param ord custom ordering.
      * @return sorted and concatenated string representation of this edge set.
      */
    def asSortedString(separator: String = GraphBase.defaultSeparator)(implicit
        ord: EdgeOrdering = defaultEdgeOrdering
    ) =
      toList.sorted(ord) mkString separator

    /** Sorts all edges according to `ord`, concatenates them using `separator`
      * and prefixes and parenthesizes the result with `stringPrefix`.
      *
      * @param separator to separate edges by.
      * @param ord custom ordering.
      * @return sorted, concatenated and prefixed string representation of this edge set.
      */
    def toSortedString(separator: String = GraphBase.defaultSeparator)(implicit
        ord: EdgeOrdering = defaultEdgeOrdering
    ): String =
      stringPrefix + "(" + asSortedString(separator)(ord) + ")"

    /** Finds the inner edge corresponding to `outerEdge`.
      *
      * @return the inner node wrapped by `Some` if found, otherwise None.
      */
    def find(outerEdge: E): Option[EdgeT]

    /** The maximum arity of all edges in this edge set. */
    def maxArity: Int = if (size == 0) 0 else max(BaseInnerEdge.ArityOrdering).arity

    /** `Iterator` over this `EdgeSet` mapped to outer edges. */
    @inline final def outerIterator: Iterator[E] = iterator map (_.outer)

    /** `Iterable` over this `EdgeSet` mapped to outer edges. */
    @inline final def outerIterable: Iterable[E] = map(_.outer)

    /** Converts this edge set to a set of outer edges. */
    def toOuter: Set[E] = {
      val b = Set.newBuilder[E]
      this foreach (b += _.outer)
      b.result()
    }

    final def draw(random: Random): EdgeT = (nodes draw random).edges draw random

    final def findElem[B](other: B, correspond: (EdgeT, B) => Boolean): EdgeT = {
      def find(edge: E): EdgeT = correspond match {
        case c: ((EdgeT, E) => Boolean) @unchecked => nodes.lookup(edge._n(0)).edges findElem (edge, c)
        case _                                     => throw new IllegalArgumentException
      }
      other match {
        case OuterEdge(e)     => find(e)
        case e: BaseInnerEdge => find(e.asInstanceOf[EdgeT].outer)
        case _                => null.asInstanceOf[EdgeT]
      }
    }

    def diff(that: AnySet[EdgeT]): AnySet[EdgeT] = this.toSet diff that
  }

  /** The edge set of this `Graph` commonly referred to as E(G).
    *
    * @return Set of all contained edges.
    */
  def edges: EdgeSetT
  def totalWeight: Double = edges.foldLeft(0d)(_ + _.weight)
}

object GraphBase {
  val defaultSeparator = ", "
}
