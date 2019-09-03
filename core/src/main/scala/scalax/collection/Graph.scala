package scalax.collection

import language.higherKinds
import collection.{IterableOnce, SetOps}
import scala.reflect.ClassTag

import GraphPredef.{EdgeLikeIn, InnerEdgeParam, InnerNodeParam, OuterEdge, OuterNode, Param}
import GraphEdge.{DiHyperEdgeLike, Keyed, UnDiEdge}
import generic.{GraphCompanion, GraphCoreCompanion}
import config.GraphConfig

/** A template trait for graphs.
  *
  * This trait provides the common structure and operations of immutable graphs independently
  * of their representation.
  *
  * If `E` inherits `DiHyperEdgeLike` the graph is directed, otherwise it is undirected or mixed.
  *
  * @tparam N    the user type of the nodes (vertices) in this graph.
  * @tparam E    the higher kinded type of the edges (links) in this graph.
  * @tparam This the higher kinded type of the graph itself.
  *
  * @define REIMPLFACTORY Note that this method must be reimplemented in each module
  *         having its own factory methods such as `constrained` does.
  * @define CONTGRAPH The `Graph` instance that contains `this`
  * @author Peter Empen
  */
trait GraphLike[N,
                E[+X] <: EdgeLikeIn[X],
                +This[NN, EE[+XX] <: EdgeLikeIn[XX]] <: GraphLike[NN, EE, This] with AnySet[Param[NN, EE]] with Graph[NN, EE]]
    extends AnySet[Param[N, E]]
    with SetOps[Param[N, E], AnySet, This[N, E]]
    with GraphTraversal[N, E]
    with GraphBase[N, E]
    with GraphDegree[N, E] {
  //thisGraph: This[N,E] => // see https://youtrack.jetbrains.com/issue/SCL-13199
  thisGraph: This[N, E] with GraphLike[N, E, This] with AnySet[Param[N, E]] with Graph[N, E] =>

  protected type ThisGraph = thisGraph.type
  implicit val edgeT: ClassTag[E[N]]

  def isDirected: Boolean         = isDirectedT || edges.hasOnlyDiEdges
  final protected val isDirectedT = classOf[DiHyperEdgeLike[_]].isAssignableFrom(edgeT.runtimeClass)

  def isHyper: Boolean         = isHyperT && edges.hasAnyHyperEdge
  final protected val isHyperT = !classOf[UnDiEdge[_]].isAssignableFrom(edgeT.runtimeClass)

  def isMixed: Boolean = !isDirectedT && edges.hasMixedEdges

  def isMulti: Boolean         = isMultiT || edges.hasAnyMultiEdge
  final protected val isMultiT = classOf[Keyed].isAssignableFrom(edgeT.runtimeClass)

  /** The companion object of `This`. */
  val graphCompanion: GraphCompanion[This]
  protected type Config <: GraphConfig
  implicit def config: graphCompanion.Config with Config

  override def empty = graphCompanion.empty
  override protected def fromSpecific(coll: IterableOnce[Param[N, E]]) = ??? // graphCompanion.from(...toOuterNodes, ...toOuterEdges)
  override protected def newSpecificBuilder = graphCompanion.newBuilder

  override def stringPrefix: String = "Graph"

  /** Ensures sorted nodes/edges unless this `Graph` has more than 100 elements.
    * See also `asSortedString` and `toSortedString`.
    */
  override def toString = if (size <= 100) toSortedString()()
  else super.toString

  /** Sorts all nodes of this graph by `ordNode` followed by all edges sorted by `ordEdge`
    * and concatinates their string representation `nodeSeparator` and `edgeSeparator`
    * respectively.
    *
    * @param nodeSeparator to separate nodes by.
    * @param edgeSeparator to separate edges by.
    * @param nodesEdgesSeparator to separate nodes from edges by.
    * @param withNodesEdgesPrefix whether the node and edge set should be prefixed.
    * @param ordNode the node ordering defaulting to `defaultNodeOrdering`.
    * @param ordEdge the edge ordering defaulting to `defaultEdgeOrdering`.
    */
  def asSortedString(nodeSeparator: String = GraphBase.defaultSeparator,
                     edgeSeparator: String = GraphBase.defaultSeparator,
                     nodesEdgesSeparator: String = GraphBase.defaultSeparator,
                     withNodesEdgesPrefix: Boolean = false)(implicit ordNode: NodeOrdering = defaultNodeOrdering,
                                                            ordEdge: EdgeOrdering = defaultEdgeOrdering) = {
    val ns =
      if (withNodesEdgesPrefix) nodes.toSortedString(nodeSeparator)(ordNode)
      else nodes.asSortedString(nodeSeparator)(ordNode)
    val es =
      if (withNodesEdgesPrefix) edges.toSortedString(edgeSeparator)(ordEdge)
      else edges.asSortedString(edgeSeparator)(ordEdge)
    ns + (if (ns.length > 0 && es.length > 0) nodesEdgesSeparator
          else "") +
    es
  }

  /** Same as `asSortedString` but additionally prefixed and parenthesized by `stringPrefix`.
    */
  def toSortedString(nodeSeparator: String = GraphBase.defaultSeparator,
                     edgeSeparator: String = GraphBase.defaultSeparator,
                     nodesEdgesSeparator: String = GraphBase.defaultSeparator,
                     withNodesEdgesPrefix: Boolean = false)(implicit ordNode: NodeOrdering = defaultNodeOrdering,
                                                            ordEdge: EdgeOrdering = defaultEdgeOrdering) =
    stringPrefix +
      "(" + asSortedString(nodeSeparator, edgeSeparator, nodesEdgesSeparator, withNodesEdgesPrefix)(ordNode, ordEdge) +
      ")"

  /** `Graph` instances are equal if their nodes and edges turned
    * to outer nodes and outer edges are equal. Any `TraversableOnce`
    * instance may also be equal to this graph if its set representation
    * contains equalling outer nodes and outer edges. Thus the following
    * expressions hold:
    * {{{
    * Graph(1~2, 3) == List(1~2, 3)
    * Graph(1~2, 3) == List(1, 2, 2, 3, 2~1)
    * }}}
    * The first test is `false` because of the failing nodes `1` and `2`.
    * The second is true because of duplicate elimination and undirected edge equivalence.
    */
  override def equals(that: Any): Boolean = that match {
    case that: Graph[N, E] =>
      (this eq that) ||
        (this.order == that.order) &&
          (this.graphSize == that.graphSize) && {
          val thatNodes = that.nodes.toOuter
          try this.nodes forall (thisN => thatNodes(thisN.value))
          catch { case _: ClassCastException => false }
        } && {
          val thatEdges = that.edges.toOuter
          try this.edges forall (thisE => thatEdges(thisE.toOuter))
          catch { case _: ClassCastException => false }
        }
    case that: TraversableOnce[_] =>
      val thatSet = that.toSet
      (this.size == thatSet.size) && {
        val thatNodes = thatSet.asInstanceOf[Set[N]]
        try this.nodes forall (thisN => thatNodes(thisN.value))
        catch { case _: ClassCastException => false }
      } && {
        val thatEdges = thatSet.asInstanceOf[Set[E[N]]]
        try this.edges forall (thisE => thatEdges(thisE.toOuter))
        catch { case _: ClassCastException => false }
      }
    case _ =>
      false
  }

  type NodeT <: InnerNode
  trait InnerNode extends super.InnerNode with TraverserInnerNode {
    this: NodeT =>

    /** $CONTGRAPH inner edge. */
    final def containingGraph: ThisGraph = thisGraph
  }
  abstract protected class NodeBase(override val value: N)
      extends super.NodeBase
      with InnerNodeParam[N]
      with InnerNode {
    this: NodeT =>
    final def isContaining[N, E[+X] <: EdgeLikeIn[X]](g: GraphBase[N, E]): Boolean =
      g eq containingGraph
  }

  type NodeSetT <: NodeSet
  trait NodeSet extends super.NodeSet {
    protected def copy: NodeSetT

    final override def -(node: NodeT): NodeSetT =
      if (this contains node) { val c = copy; c minus node; c } else this.asInstanceOf[NodeSetT]

    /** removes `node` from this node set leaving the edge set unchanged.
      *
      * @param node the node to be removed from the node set.
      */
    protected def minus(node: NodeT): Unit

    /** removes `node` either rippling or gently.
      *
      * @param node the node to be subtracted
      * @param rippleDelete if `true`, `node` will be deleted with its incident edges;
      *        otherwise `node` will be only deleted if it has no incident edges or
      *        all its incident edges are hooks.
      * @param minusNode implementation of node removal without considering incident edges.
      * @param minusEdges implementation of removal of all incident edges.
      * @return `true` if `node` has been removed.
      */
    final protected[collection] def subtract(node: NodeT,
                                             rippleDelete: Boolean,
                                             minusNode: (NodeT) => Unit,
                                             minusEdges: (NodeT) => Unit): Boolean = {
      def minusNodeTrue = { minusNode(node); true }
      def minusAllTrue  = { minusEdges(node); minusNodeTrue }
      if (contains(node))
        if (node.edges.isEmpty) minusNodeTrue
        else if (rippleDelete) minusAllTrue
        else if (node.hasOnlyHooks) minusAllTrue
        else handleNotGentlyRemovable
      else false
    }
    protected def handleNotGentlyRemovable = false
  }

  trait InnerEdge extends super.InnerEdge {
    this: EdgeT =>

    /** $CONTGRAPH inner edge. */
    final def containingGraph: ThisGraph = thisGraph
  }
  type EdgeT <: InnerEdgeParam[N, E, NodeT, E] with InnerEdge with Serializable
  class EdgeBase(override val edge: E[NodeT]) extends InnerEdgeParam[N, E, NodeT, E] with InnerEdge {
    this: EdgeT =>
    override def iterator: Iterator[NodeT] = edge.iterator.asInstanceOf[Iterator[NodeT]]
    override def stringPrefix              = super.stringPrefix
  }
  type EdgeSetT <: EdgeSet
  trait EdgeSet extends super.EdgeSet {
    def hasOnlyDiEdges: Boolean
    def hasOnlyUnDiEdges: Boolean
    def hasMixedEdges: Boolean
    def hasAnyHyperEdge: Boolean
    def hasAnyMultiEdge: Boolean
  }

  /** Checks whether a given node or edge is contained in this graph.
    *
    *  @param elem the node or edge the existence of which is to be checked
    *  @return true if `elem` is contained in this graph
    */
  def contains(elem: Param[N, E]) = elem match {
    case n: OuterNode[N]    => nodes contains newNode(n.value)
    case e: OuterEdge[N, E] => edges contains newEdge(e.edge)
    case n: InnerNodeParam[N] =>
      val node = n.toNodeT[N, E, ThisGraph](thisGraph)(anyNode => newNode(anyNode.value))
      nodes contains node
    case e: InnerEdgeParam[N, E, _, E] =>
      val edge = e.toEdgeT[N, E, ThisGraph](thisGraph)(anyEdge => newEdge(anyEdge.toOuter))
      edges contains edge
  }

  /** Iterates over all nodes and all edges.
    *
    *  @return iterator containing all nodes and all edges
    */
  def iterator: Iterator[Param[N, E]] = nodes.iterator ++ edges.iterator

  /** Searches for an inner node equaling to `outerNode` in this graph.
    *
    * @param outerNode the outer node to search for in this graph.
    * @return `Some` of the inner node found or `None`.
    */
  @inline final def find(outerNode: N): Option[NodeT] = nodes find outerNode

  /** Searches for an edge node equaling to `outerEdge` in this graph.
    *
    * @param outerEdge the outer edge to search for in this graph.
    * @return `Some` of the inner edge found or `None`.
    */
  @inline final def find(outerEdge: E[N]): Option[EdgeT] = edges find outerEdge

  /** Searches for an inner node equaling to `outerNode` in this graph
    * which must exist in this graph.
    *
    * @param outerNode the outer node to search for in this graph.
    * @return the inner node if found. Otherwise NoSuchElementException is thrown.
    */
  @inline final def get(outerNode: N): NodeT = nodes get outerNode

  /** Searches for an inner edge equaling to `outerEdge` in this graph
    * which must exist in this graph.
    *
    * @param outerEdge the outer edge to search for in this graph.
    * @return the inner edge if found. Otherwise NoSuchElementException is thrown.
    */
  @inline final def get(outerEdge: E[N]) = find(outerEdge).get

  /** Searches for an inner node equaling to `outerNode` in this graph.
    *
    * @param outerNode the outer node to search for in this graph.
    * @param default the inner node to return if `outerNode` is not contained.
    * @return The inner node looked up or `default` if no inner node
    *         equaling to `outerNode` could be found.
    */
  @inline final def getOrElse(outerNode: N, default: NodeT) = find(outerNode).getOrElse(default)

  /** Searches for an inner edge equaling to `outerEdge` in this graph.
    *
    * @param outerEdge the outer edge to search for in this graph.
    * @param default the inner edge to return if `outerEdge` cannot be found.
    * @return the inner edge looked up or `default` if no inner edge
    *         equaling to `edge` could be found.
    */
  @inline final def getOrElse(outerEdge: E[N], default: EdgeT) =
    find(outerEdge).getOrElse(default)

  /** Creates a new supergraph with an additional node, unless the node passed is
    *  already present.
    *
    *  @param node the node to be added
    *  @return the new supergraph containing all nodes and edges of this graph and `node`
    *  additionally.
    */
  def +(node: N): This[N, E]

  /** Creates a new supergraph with an additional edge, unless the edge passed is
    *  already present.
    *
    *  @param edge the edge to be added
    *  @return the new supergraph containing all nodes and edges of this
    *          graph plus `edge`.
    */
  protected def +#(edge: E[N]): This[N, E]

  /** Creates a new supergraph with an additional node or edge, unless the
    *  node or edge passed is already present.
    *
    *  This method purely wraps `+(node: N)` respectively `+#(edge: E[N])`
    *  granting the same behavior.
    *
    *  @param elem the wrapped node or edge to be added; if `elem` is of type N,
    *         the wrapped object is added to the node set otherwise to the edge set.
    *  @return a new supergraph containing all nodes and edges of this graph
    *          plus `elem`.
    */
  def incl(elem: Param[N, E]): This[N, E] = elem match {
    case n: OuterNode[N]    => this + n.value
    case e: OuterEdge[N, E] => this +# e.edge
    case n: InnerNodeParam[N] => this + n.value
    case e: InnerEdgeParam[N, E, _, E] => this +# e.asEdgeT[N, E, ThisGraph](thisGraph).toOuter
  }
  override def concat(elems: IterableOnce[Param[N, E]]): This[N, E] = bulkOp(elems, isPlusPlus = true)
  override def --(elems: IterableOnce[Param[N, E]]): This[N, E] = bulkOp(elems, isPlusPlus = false)
  override def diff(that: AnySet[Param[N, E]]): This[N, E] = ??? // TODO is this the same as --???

  /** Prepares and calls `plusPlus` or `minusMinus`. */
  final protected def bulkOp(elems: IterableOnce[Param[N, E]], isPlusPlus: Boolean): This[N, E] = {
    val p = partition(elems)
    if (isPlusPlus) plusPlus(p.toOuterNodes, p.toOuterEdges)
    else minusMinus(p.toOuterNodes, p.toOuterEdges)
  }

  final protected def partition(elems: IterableOnce[Param[N, E]]) =
    new Param.Partitions[N, E](elems match {
      case t: Iterable[Param[N, E]]     => t
      case g: IterableOnce[Param[N, E]] => g.iterator.to(Iterable)
      case _                            => throw new MatchError("Iterable expected.")
    })

  /** Implements the heart of `++` calling the `from` factory method of the companion object.
    *  $REIMPLFACTORY */
  protected def plusPlus(newNodes: Iterable[N], newEdges: Iterable[E[N]]): This[N, E] =
    graphCompanion.from[N, E](nodes.toOuter ++ newNodes, edges.toOuter ++ newEdges)

  /** Implements the heart of `--` calling the `from` factory method of the companion object.
    *  $REIMPLFACTORY */
  protected def minusMinus(delNodes: Iterable[N], delEdges: Iterable[E[N]]): This[N, E] = {
    val delNodesEdges = minusMinusNodesEdges(delNodes, delEdges)
    graphCompanion.from[N, E](delNodesEdges._1, delNodesEdges._2)
  }

  /** Calculates the `nodes` and `edges` arguments to be passed to a factory method
    *  when delNodes and delEdges are to be deleted by `--`.
    */
  protected def minusMinusNodesEdges(delNodes: Iterable[N], delEdges: Iterable[E[N]]): (Set[N], Set[E[N]]) =
    (nodes.toOuter -- delNodes, {
      val delNodeSet = delNodes.toSet
      val restEdges =
        for (e <- edges.toOuter if e forall (n => !(delNodeSet contains n))) yield e
      restEdges -- delEdges
    })

  /** Creates a new subgraph consisting of all nodes and edges of this graph except `node`
    *  and those edges which `node` is incident with.
    *
    *  @param node the node to be removed.
    *  @return the new subgraph of this graph after the "ripple" deletion of `node`.
    */
  def -(node: N): This[N, E]

  /** Creates a new subgraph consisting of all nodes and edges of this graph except `node`
    *  which is conditionally removed from this graph. The removal only succeeds if the node
    *  is not incident with any edges or it is only incident with hooks.
    *
    *  @param node the node to be gently removed.
    *  @return the new subgraph of this graph after the "gentle" deletion of `node`.
    *          If `node` could not be deleted, the new graph is a copy of this graph.
    */
  def minusIsolated(node: N): This[N, E]

  /** Creates a new subgraph consisting of all nodes and edges of this graph except `edge`.
    * The node set remains unchanged.
    *
    *  @param edge the edge to be removed.
    *  @return a new subgraph of this graph that contains all nodes and edges of this graph
    *          except of `edge`.
    */
  protected def -#(edge: E[N]): This[N, E]

  /** Creates a new subgraph consisting of all nodes and edges of this graph except `edge`
    *  and those nodes which are incident with `edge` and would become edge-less after deletion.
    *
    *  @param edge the edge to be removed.
    *  @return a new subgraph of this graph after the "ripple" deletion of `edge` from
    *          this graph.
    */
  protected def -!#(edge: E[N]): This[N, E]

  /** Creates a new subgraph consisting of all nodes and edges of this graph except `elem`.
    *  If `elem` is of type N, this method maps to `-(node: N)`. Otherwise the edge is deleted
    *  leaving the node set unchanged.
    *
    *  @param elem node or edge to be removed.
    *  @return the new subgraph of this graph after the "ripple" deletion of the passed node
    *          or the simple deletion of the passed edge.
    */
  def excl(elem: Param[N, E]): This[N, E] = elem match {
    case n: OuterNode[N]               => this - n.value
    case e: OuterEdge[N, E]            => this -# e.edge
    case n: InnerNodeParam[N]          => this - n.value
    case e: InnerEdgeParam[N, E, _, E] => this -# e.asEdgeT[N, E, ThisGraph](thisGraph).toOuter
  }

  /** Creates a new subgraph consisting of all nodes and edges of this graph except `elem`.
    *  If `elem` is of type N, this method maps to `-(node: N)`. Otherwise the edge is deleted
    *  along with those incident nodes which would become edge-less after deletion.
    *
    *  @param elem node or edge to be removed.
    *  @return a new subgraph of this graph after the "ripple" deletion of the passed
    *          node or edge.
    */
  def -!(elem: Param[N, E]): This[N, E] = elem match {
    case n: OuterNode[N]               => this - n.value
    case e: OuterEdge[N, E]            => this -!# e.edge
    case n: InnerNodeParam[N]          => this - n.value
    case e: InnerEdgeParam[N, E, _, E] => this -!# e.asEdgeT[N, E, ThisGraph](thisGraph).toOuter
  }

  /** Creates a new subgraph consisting of all nodes and edges of this graph but the elements
    * of `coll` which will be unconditionally removed. This operation differs from `--`
    * in that edges are deleted along with those incident nodes which would become isolated
    * after deletion.
    *
    * @param elems collection of nodes and/or edges to be removed; if the element type is N,
    *             it is removed from the node set otherwise from the edge set.
    *             See `-!(elem: Param[N,E])`.
    * @return the new subgraph containing all nodes and edges of this graph
    *         after the "ripple" deletion of nodes and the simple deletion of edges in `coll` .
    */
  def --!(elems: IterableOnce[Param[N, E]]): This[N, E] = {
    val p                    = partition(elems)
    val (delNodes, delEdges) = (p.toOuterNodes, p.toOuterEdges)
    val unconnectedNodeCandidates = {
      val edgeNodes = MSet.empty[N]
      delEdges foreach (_ foreach (n => edgeNodes += n))
      edgeNodes -- delNodes
    }
    val delEdgeSet = {
      val edges = MSet.empty[EdgeT]
      delEdges foreach (this find _ map (edges += _))
      edges
    }
    minusMinus(
      delNodes ++
        (unconnectedNodeCandidates filter (nc => this find nc exists (n => n.edges forall (delEdgeSet contains _)))),
      delEdges)
  }

  /** Provides a shortcut for predicates involving any graph element.
    * In order to compute a subgraph of this graph, the result of this method
    * may be passed to any graph-level method requiring a predicate such as
    * `count`, `exists`, `filter`, `filterNot`, `forall` etc. For instance
    *
    * {{{
    * val g = Graph(2~>3, 3~>1, 5)
    * g filter g.having(nodes = _ >= 2) // yields Graph(2, 3, 5, 2~>3)
    * }}}
    *
    * @param node The predicate that must hold for the nodes.
    * @param edge The predicate that must hold for the edges. If omitted, all edges
    *             between nodes to be included by `nodes` will also be included.
    * @return A partial function combining the passed predicates.
    */
  def having(node: NodeFilter = _ => false, edge: EdgeFilter = null): PartialFunction[Param[N, E], Boolean] = {
    val nodePred: PartialFunction[Param[N, E], Boolean] = {
      case n: InnerNodeParam[N] => node(n.asNodeT[N, E, ThisGraph](thisGraph))
    }
    val edgePred: PartialFunction[Param[N, E], Boolean] =
      if (edge eq null) {
        case e: InnerEdgeParam[N, E, _, E] => e.asEdgeT[N, E, ThisGraph](thisGraph) forall (node(_))
      } else {
        case e: InnerEdgeParam[N, E, _, E] => edge(e.asEdgeT[N, E, ThisGraph](thisGraph))
      }
    nodePred orElse edgePred
  }

  // TODO I don't see a way to have map return Graph[M, F] when B = Param[M, F]...
  // override def map[B](f: Param[N, E] => B): AnySet[B] = ???

  // TODO but we could do something like this instead
  //def mapGraph[M,F](f: Param[N, E] => Param[M, F]): Graph[M, F] = this.view.map(f).to(Graph)
}

// ----------------------------------------------------------------------------

/** The main trait for immutable graphs bundling the functionality of traits concerned with
  * specific aspects.
  *
  * @tparam N the type of the nodes (vertices) in this graph.
  * @tparam E the kind of the edges in this graph.
  *
  * @author Peter Empen
  */
trait Graph[N, E[+X] <: EdgeLikeIn[X]] extends AnySet[Param[N, E]] with GraphLike[N, E, Graph] {
  override def empty: Graph[N, E] = Graph.empty[N, E]
}

/** The main companion object for immutable graphs.
  *
  * @author Peter Empen
  */
object Graph extends GraphCoreCompanion[Graph] {
  override def newBuilder[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config) =
    immutable.Graph.newBuilder[N, E](edgeT, config)
  def empty[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig): Graph[N, E] =
    immutable.Graph.empty[N, E](edgeT, config)
  def from[N, E[+X] <: EdgeLikeIn[X]](nodes: Iterable[N] = Nil, edges: Iterable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config = defaultConfig): Graph[N, E] =
    immutable.Graph.from[N, E](nodes, edges)(edgeT, config)
/*
  // TODO build from
  implicit def cbfUnDi[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig) =
    new GraphCanBuildFrom[N, E]()(edgeT, config)
      .asInstanceOf[GraphCanBuildFrom[N, E] with CanBuildFrom[Graph[_, UnDiEdge], Param[N, E], Graph[N, E]]]

  implicit def cbfDi[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig) =
    new GraphCanBuildFrom[N, E]()(edgeT, config)
      .asInstanceOf[GraphCanBuildFrom[N, E] with CanBuildFrom[Graph[_, DiEdge], Param[N, E], Graph[N, E]]]
 */
}

