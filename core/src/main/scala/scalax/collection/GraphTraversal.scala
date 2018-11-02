package scalax.collection

import scala.annotation.tailrec
import scala.collection.{AbstractIterable, AbstractTraversable, EqSetFacade}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.language.{higherKinds, implicitConversions}
import scala.math.{max, min}

import scalax.collection.GraphPredef.{EdgeLikeIn, OuterElem}
import scalax.collection.mutable.{EqHashMap, EqHashSet}
import scalax.collection.generic.GraphCoreCompanion

/** Graph-related functionality such as traversals, path finding, cycle detection etc.
  *  All algorithms including breadth-first, depth-first, white-gray-black search and
  *  Dijkstra's algorithm are tail recursive.
  *
  * Before starting a traversal a `Traverser` such as [[scalax.collection.GraphTraversal#InnerNodeTraverser]]
  * is instantiated explicitly or implicitly. It holds settings like `maxDepth`, `subgraph` or `ordering`
  * providing a fine-grained control of the traversal. `Traverser`s also extend
  * `scala.collection.Traversable` meaning that you can process the visited nodes and edges
  * in a functional way.
  *
  * @see [[http://www.scala-graph.org/guides/core-traversing]]
  *
  * @define INTOACC taking optional properties like
  *         subgraph restriction, ordering or maximum depth into account.
  * @define EXTNODEVISITOR Alternatively, an instance of `ExtendedNodeVisitor`
  *         may be passed to obtain additional state information such as the current
  *         depth. The concrete type of the last argument, the informer
  *         depends on the underlying implementation so you need to match against it.
  * @define ORD If a `NodeOrdering` or `EdgeOrdering` different from `NoOrdering` is supplied
  *         neighbor nodes will visited during the traversal according to this ordering.
  * @define MAXWEIGHT An optional maximum weight that limits the scope of the traversal or search.
  *         If defined and the sum of edge weights between the root of the traversal and a node
  *         exceeds the given maximum, that node will no more be visited.
  * @define CONSIDERING considering all traversal properties passed to the traverser
  *         factory method like [[scalax.collection.GraphTraversal#innerNodeTraverser]]
  *         or altered by any `with*` method.
  * @define OPTVISITOR An optional function that is applied for its side-effect to
  *         every element visited during graph traversal.
  * @define EXTENDSTYPE which extends `scala.collection.Traversable` with elements of type
  * @define SETROOT and sets its `root` to this node
  * @define TOSTART To start a traversal call one of the graph traversal methods or
  *         any appropriate method inherited from [[scala.collection.Traversable]] on this
  *         instance.
  * @define ROOT The node where subsequent graph traversals start.
  * @define PARAMETERS The properties controlling subsequent traversals.
  * @define SUBGRAPHNODES Restricts subsequent graph traversals to visit only nodes
  *         holding this predicate.
  * @define SUBGRAPHEDGES Restricts subsequent graph traversals to walk only along edges
  *         that hold this predicate.
  * @define DOWNUPBOOLEAN where the `Boolean` parameter is `true` if the traversal takes
  *         place in downward and `false` if it takes place in upward direction.
  * @define PATHSYNTAX `::= ''node'' { ''edge'' ''node'' }`
  * @define WALKPATH A walk/path contains at least one node followed by any number of
  *         consecutive pairs of an edge and a node.
  *         The first element is the start node, the second is an edge with its source
  *         being the start node and its target being the third element etc.
  * @define SANECHECK This optional check is sane if there is reasonable doubt
  *         about the correctness of some algorithm results.
  * @define BUILDERADDS Nodes and edges may be added either alternating or node by node
  *         respectively edge by edge. Either way, the builder ensures that the added
  *         elements build a valid
  * @define BUILDERREC It is recommended using `add` instead of `+=` to track failed
  *         additions.
  * @define EDGESELECTOR Determines the edge to be selected between neighbor nodes
  *         if an edge is not supplied explicitly. This is only relevant in case of
  *         multigraphs.
  * @define SEEFLUENT See `componentTraverser` for more control by means of `FluentProperties`.
  * @define SORTVISITOR called for each inner node or inner edge visited during the sort.
  *
  * @author Peter Empen
  */
trait GraphTraversal[N, E[X] <: EdgeLikeIn[X]] extends GraphBase[N, E] {
  thisGraph =>

  import GraphTraversal._
  import Visitor._

  /** Whether `this` graph is connected if it is undirected or
    *  weakly connected if it is directed.
    */
  def isConnected: Boolean = nodes.headOption forall {
    _.innerNodeTraverser(Parameters(kind = DepthFirst, direction = AnyConnected)).size == nodes.size
  }

  /** Whether `this` graph has at least one cycle in any of its components.
    */
  @inline final def isCyclic: Boolean = findCycle().isDefined

  /** Whether `this` graph has no cycle.
    */
  @inline final def isAcyclic: Boolean = !isCyclic

  /** Finds a cycle in `this` graph in any of its components
    *  and calls `visitor` for each inner element visited during the search.
    *  $SEEFLUENT
    */
  final def findCycle[U](implicit visitor: InnerElem => U = empty): Option[Cycle] =
    componentTraverser().findCycle(visitor)

  /** Finds a cycle that contains `node`
    *  and calls `visitor` for each inner element visited during the search.
    */
  final def findCycleContaining[U](node: NodeT)(implicit visitor: InnerElem => U = empty): Option[Cycle] =
    innerNodeTraverser(node).partOfCycle()

  /** Represents a topological sort layer. */
  case class Layer protected[collection] (index: Int, nodes: IndexedSeq[NodeT])

  /** The result of a topological sort in the layered view. */
  type Layers = Traversable[Layer]

  /** Topologically ordered nodes or layers of a topological order of a graph or of an isolated graph component.
    *  @tparam A one of `NodeT`, `N`
    *  @tparam T one of `A` or `(Int, Iterable[A])`
    *  @define NEWFLAVOR Creates a new flavor of this `TopologicalOrder` or `LayeredTopologicalOrder` */
  sealed abstract class AbstractTopologicalOrder[+A, +T] extends AbstractTraversable[T] {

    protected val layers: Layers
    protected def toA: NodeT => A

    def layerOrdering: NodeOrdering
    protected def ordered(nodes: IndexedSeq[NodeT]): IndexedSeq[NodeT] =
      if (layerOrdering.isDefined) nodes.sorted(layerOrdering)
      else nodes

    /** The number of layers of this topological order. */
    def nrOfLayers = layers.size

    /** $NEWFLAVOR with nodes ordered by `newOrdering` within the layers.
      *  A layer ordering is also useful to ensure a stable topological order over graph instances. */
    def withLayerOrdering(newOrdering: NodeOrdering): AbstractTopologicalOrder[A, T]

    /** $NEWFLAVOR that is traversable for its inner nodes zipped with their layers. */
    def toLayered: LayeredTopologicalOrder[A]

    override def hashCode: Int = layers.##

    override def equals(other: Any): Boolean = other match {
      case that: AbstractTopologicalOrder[_, _] =>
        (this.layers eq that.layers) ||
          this.layers == that.layers
      case _ => false
    }

    override def stringPrefix = getClass.getSimpleName
  }

  /** A traversable topological order of nodes of a graph or of an isolated graph component.
    *  @tparam A one of `NodeT`, `N` */
  final class TopologicalOrder[+A] protected[collection] (
      override protected val layers: Layers,
      override protected val toA: NodeT => A)(implicit override val layerOrdering: NodeOrdering = NodeOrdering.None)
      extends AbstractTopologicalOrder[A, A] {

    def foreach[U](f: A => U): Unit =
      for {
        layer <- layers
        node  <- ordered(layer.nodes)
      } f(toA(node))

    def withLayerOrdering(newOrdering: NodeOrdering): TopologicalOrder[A] =
      new TopologicalOrder(layers, toA)(newOrdering)

    def toOuter: TopologicalOrder[N] =
      new TopologicalOrder(layers, _.value)(layerOrdering)

    def toLayered: LayeredTopologicalOrder[A] =
      new LayeredTopologicalOrder[A](layers, toA)(layerOrdering)
  }

  /** Layers of a topological order of a graph or of an isolated graph component.
    *  The layers of a topological sort can roughly be defined as follows:
    *      a. layer 0 contains all nodes having no predecessors,
    *      a. layer n contains those nodes that have only predecessors in anchestor layers
    *         with at least one of them contained in layer n - 1
    *  @tparam A one of `NodeT`, `N` */
  final class LayeredTopologicalOrder[+A] protected[collection] (
      override protected val layers: Layers,
      override protected val toA: NodeT => A)(implicit override val layerOrdering: NodeOrdering = NodeOrdering.None)
      extends AbstractTopologicalOrder[A, (Int, Iterable[A])] {

    def foreach[U](f: Tuple2[Int, Iterable[A]] => U): Unit =
      for (layer <- layers) f(layer.index -> toIterable(layer.nodes))

    def withLayerOrdering(newOrdering: NodeOrdering): LayeredTopologicalOrder[A] =
      new LayeredTopologicalOrder(layers, toA)(newOrdering)

    def toOuter: LayeredTopologicalOrder[N] =
      new LayeredTopologicalOrder(layers, _.value)(layerOrdering)

    def toLayered: LayeredTopologicalOrder[A] = this

    // O(1) view to avoid exposure of the possibly mutable `layers`
    private def toIterable(iSeq: IndexedSeq[NodeT]): Iterable[A] = new AbstractIterable[A] {
      def iterator: Iterator[A] = new AbstractIterator[A] {
        private val it       = ordered(iSeq).toIterator
        def hasNext: Boolean = it.hasNext
        def next(): A        = toA(it.next)
      }
      override def stringPrefix = "Nodes"
    }
  }

  /** Either a `Right` containing a valid topological order or a `Left` containing a node on a cycle. */
  type CycleNodeOrTopologicalOrder = Either[NodeT, TopologicalOrder[NodeT]]

  /** Sorts this graph topologically.
    *  @param visitor $SORTVISITOR
    *  $SEEFLUENT */
  final def topologicalSort[U](implicit visitor: InnerElem => U = empty): CycleNodeOrTopologicalOrder =
    componentTraverser().topologicalSort(visitor)

  /** Sorts every isolated component of this graph topologically.
    *  @param visitor $SORTVISITOR
    *  $SEEFLUENT */
  final def topologicalSortByComponent[U](
      implicit visitor: InnerElem => U = empty): Traversable[CycleNodeOrTopologicalOrder] =
    componentTraverser().topologicalSortByComponent(visitor)

  @inline final protected def defaultPathSize: Int = min(256, nodes.size * 2)

  /** Represents a walk in this graph where `walk` $PATHSYNTAX
    * $WALKPATH
    * @define CUMWEIGHT The cumulated weight of all edges on this path/walk.
    */
  trait Walk extends Traversable[InnerElem] {
    override def stringPrefix = "Walk"

    /** All nodes on this path/walk in proper order. */
    def nodes: Traversable[NodeT]

    /** All edges of this path/walk in proper order. */
    def edges: Traversable[EdgeT]

    /** $CUMWEIGHT */
    final def weight: Double = (0d /: edges)((sum, edge) => sum + edge.weight)

    /** $CUMWEIGHT
      *  @param f The weight function overriding edge weights. */
    final def weight[T: Numeric](f: EdgeT => T): T = {
      val num = implicitly[Numeric[T]]
      import num._
      (num.zero /: edges)((sum, edge) => sum + f(edge))
    }

    /** The number of edges on this path/walk. */
    def length: Int = nodes.size - 1

    /** The number of nodes and edges on this path/walk. */
    override def size: Int = 2 * length + 1

    def startNode: NodeT
    def endNode: NodeT

    def foreach[U](f: InnerElem => U): Unit = {
      f(nodes.head)
      val edges = this.edges.toIterator
      for (n <- nodes.tail;
           e = edges.next) {
        f(e)
        f(n)
      }
    }

    /** Returns whether the nodes and edges of this walk are valid with respect
      *  to this graph. $SANECHECK */
    def isValid: Boolean = {
      val valid = nodeValidator
      nodes.headOption exists { startNode =>
        val (nodesIt, edgesIt) = (nodes.tail.toIterator, edges.toIterator)

        @tailrec def ok(prev: NodeT, count: Int): Boolean =
          if (nodesIt.hasNext && edgesIt.hasNext) {
            val node = nodesIt.next
            if (valid(prev) &&
                edgesIt.next.matches((n: NodeT) => n eq prev, (n: NodeT) => n eq node))
              ok(node, count + 1)
            else false
          } else if (nodesIt.isEmpty && edgesIt.isEmpty) {
            count > 0
          } else false

        ok(startNode, 0)
      }
    }

    protected trait NodeValidator extends (NodeT => Boolean)

    protected def nodeValidator: NodeValidator =
      new NodeValidator {
        def apply(node: NodeT): Boolean = true
      }

    /** The `Graph` instance that contains `this` walk. */
    final def containingGraph: thisGraph.type = thisGraph
  }
  object Walk {
    protected[GraphTraversal] trait Zero {
      this: Walk =>
      protected def single: NodeT
      val nodes            = List(single)
      def edges            = Nil
      def startNode        = single
      def endNode          = single
      override def isValid = true
    }

    /** A walk of zero length that is a single node. */
    def zero(node: NodeT) =
      new Walk with Zero {
        final protected def single = node
      }
  }

  /** A `Builder` for valid walks in this graph.
    *
    * $BUILDERADDS walk.
    *
    * A node addition fails if the node to be added is not a direct successor of the
    * previously added node or of the target node of the previously added edge.
    * An edge addition fails if the edge to be added is not an outgoing edge from the
    * previously added node or of the target node of the previously added edge.
    *
    * $BUILDERREC
    *
    * @define ADDELEM Tries to add `elem` to the tail of the path/walk.
    * @define ADDNODE Tries to add `node` to the tail of the path/walk.
    * @define ADDEDGE Tries to add `edge` to the tail of the path/walk.
    * @define ADDSUCCESS Whether the addition was successful.
    */
  trait WalkBuilder extends Builder[InnerElem, Walk] {

    /** The node this walk starts at. */
    def start: NodeT

    /** $ADDELEM
      *  @return $ADDSUCCESS */
    @inline final def add(elem: InnerElem): Boolean = elem match {
      case n: InnerNode => this add n.asNodeT[N, E, thisGraph.type](thisGraph)
      case e: InnerEdge => this add e.asEdgeT[N, E, thisGraph.type](thisGraph)
    }

    /** $ADDNODE
      *  @return $ADDSUCCESS */
    def add(node: NodeT): Boolean

    /** $ADDEDGE
      *  @return $ADDSUCCESS */
    def add(edge: EdgeT): Boolean

    /** $ADDELEM */
    def +=(elem: InnerElem): this.type = { add(elem); this }

    /** $ADDNODE */
    @inline final def +=(node: NodeT): this.type = { add(node); this }

    /** $ADDEDGE */
    @inline final def +=(edge: EdgeT): this.type = { add(edge); this }
  }

  /** Instantiates a [[WalkBuilder]] for this graph.
    *
    * @param start The node this walk starts at.
    * @param sizeHint Expected maximum number of nodes on this walk.
    * @param edgeSelector $EDGESELECTOR
    */
  def newWalkBuilder(start: NodeT)(implicit sizeHint: Int = defaultPathSize,
                                   edgeSelector: (NodeT, NodeT) => Option[EdgeT] = anyEdgeSelector): WalkBuilder

  /** Represents a path in this graph where
    *
    * `path` $PATHSYNTAX
    *
    * Nodes and edges on the path are distinct. $WALKPATH
    */
  trait Path extends Walk {
    override def stringPrefix = "Path"

    /** Returns whether the nodes and edges on this path are valid with respect
      *  to this graph. $SANECHECK
      */
    override def isValid: Boolean = super.isValid

    override protected def nodeValidator: NodeValidator =
      new NodeValidator {
        private[this] val nodeSet       = new EqHashMap[NodeT, Null](nodes.size)
        def apply(node: NodeT): Boolean = nodeSet.put(node, null).isEmpty
      }
  }
  object Path extends Serializable {

    /** A path of zero length that is a single node. */
    def zero(node: NodeT) =
      new Path with Walk.Zero {
        final protected def single = node
      }
  }

  /** A `Builder` for valid paths in this graph.
    *
    * $BUILDERADDS path.
    *
    * A node addition fails if either the node to be added is already contained or
    * the node is not a direct successor of the previously added node or
    * of the target node of the previously added edge.
    * An edge addition fails if either the edge to be added is is already contained or
    * the edge is not an outgoing edge from the previously added node or
    * of the target node of the previously added edge.
    *
    * $BUILDERREC
    */
  trait PathBuilder extends WalkBuilder with Builder[InnerElem, Path]

  /** Instantiates a [[PathBuilder]] for this graph.
    *
    * @param start The node this path starts at.
    * @param sizeHint Expected maximum number of nodes on this path.
    * @param edgeSelector $EDGESELECTOR
    */
  def newPathBuilder(start: NodeT)(implicit sizeHint: Int = defaultPathSize,
                                   edgeSelector: (NodeT, NodeT) => Option[EdgeT] = anyEdgeSelector): PathBuilder

  /** Represents a cycle in this graph listing the nodes and connecting edges on it
    *  with the following syntax:
    *
    * `cycle ::= ''start-end-node'' { ''edge'' ''node'' } ''edge'' ''start-end-node''`
    *
    * All nodes and edges on the path are distinct except the start and end nodes that
    * are equal. A cycle contains at least a start node followed by any number of
    * consecutive pairs of an edge and a node and the end node equaling to the start node.
    * The first element is the start node, the second is an edge with its tail
    * being the start node and its head being the third element etc.
    */
  trait Cycle extends Path {
    override def stringPrefix = "Cycle"

    override def endNode: NodeT = startNode

    /** Same as `sameAs` but also comparing this cycle with any `Traversable`.
      */
    final def sameElements(that: Traversable[_]): Boolean =
      this.size == that.size && {
        val thisList = to[List]
        // thisList.indexOf(that.head) may fail due to asymmetric equality
        val idx = thisList.indexWhere(_ == that.head)
        if (idx >= 0) {
          val thisDoubled = thisList ++ thisList.tail
          val thatList    = that.to[List]
          (thisDoubled startsWith (thatList, idx)) ||
          (thisDoubled startsWith (thatList.reverse, idx))
        } else false
      }

    /** Semantically compares `this` cycle with `that` cycle. While `==` returns `true`
      *  only if the cycles contain the same elements in the same order, this comparison
      *  returns also `true` if the elements of `that` cycle can be shifted and optionally
      *  reversed such that their elements have the same order. For instance, given
      *
      * `c1 = Cycle(1-2-3-1)`, `c2 = Cycle(2-3-1-2)` and `c3 = Cycle(2-1-3-2)`
      *
      * the following expressions hold:
      *
      * `c1 != c2`, `c1 != c3` but `c1 sameAs c2` and `c1 sameAs c3`.
      */
    final def sameAs(that: GraphTraversal[N, E]#Cycle): Boolean =
      this == that || (that match {
        case that: GraphTraversal[N, E]#Cycle => sameElements(that)
        case _                                => false
      })

  }
  object Cycle {
    def findLoop(node: NodeT): Option[Cycle] =
      node.hook map { hook =>
        new Cycle {
          def nodes: Traversable[NodeT] = List(startNode, endNode)
          def edges: Traversable[EdgeT] = List(hook)
          def startNode: NodeT          = node
        }
      }
    protected[collection] def of(start: NodeT, mid: NodeT): Cycle =
      new Cycle {
        def nodes: Traversable[NodeT] = List(startNode, mid, endNode)
        def edges: Traversable[EdgeT] = {
          val out = {
            val outSet = start outgoingTo mid
            outSet find (_.directed) getOrElse outSet.head
          }
          val in = (mid outgoingTo start filterNot (_ eq out)).head
          List(out, in)
        }
        def startNode: NodeT = start
      }
  }

  /** Whether all nodes are pairwise adjacent.
    *
    * @return `true` if this graph is complete, `false` if this graph contains any
    * independent nodes.
    */
  def isComplete = {
    val orderLessOne = order - 1
    nodes forall (_.diSuccessors.size == orderLessOne)
  }

  /** An arbitrary edge between `from` and `to` that is available most efficiently.
    */
  @inline final def anyEdgeSelector(from: NodeT, to: NodeT): Option[EdgeT] =
    from findOutgoingTo to

  /** Stores a value and an edge weight function
    *  for use in weight-based traversals that may be defined by `withMaxWeight`. */
  class Weight(val value: Double, val edgeWeight: EdgeT => Double) {
    val ordering = Edge weightOrdering edgeWeight
  }
  object Weight {

    /** Creates a new `Weight` with the given `value` and weight function. */
    def apply[W](value: W, edgeWeight: EdgeT => W)(implicit num: Numeric[W]): Weight = {
      import num._
      new Weight(toDouble(value), edgeWeight andThen toDouble)
    }

    /** Creates a new `Weight` with the given `value` and the default weight function returning `edge.weight`, */
    def apply(value: Long) = new Weight(value, Edge.defaultWeight _ andThen (_.toDouble))
  }

  /** Template for extended node visitors.
    *  While the default node visitor of the type `NodeT => U`
    *  passes solely the inner node being visited, extended node visitors
    *  pass the following traversal state information:
    *  1. the inner node currently visited as with a standard node visitor
    *  1. the number of nodes visited so far and
    *  1. the current depth in terms of the underlying algorithm and
    *  1. a reference to a specific informer that may be pattern matched
    *     to collect even further data specific to the implementation.
    */
  trait ExtendedNodeVisitor[U] extends (NodeT => U) {
    def apply(n: NodeT, cnt: Int, depth: Int, inf: => NodeInformer): U
    def apply(node: NodeT) = apply(node, 0, 0, NodeInformer.empty)
  }
  object ExtendedNodeVisitor {

    /** Instantiates an extended node visitor based on 'visitor'.
      */
    def apply[N, E[X] <: EdgeLikeIn[X], U](visitor: (NodeT, Int, Int, => NodeInformer) => U) =
      new ExtendedNodeVisitor[U] {
        def apply(n: NodeT, cnt: Int, depth: Int, inf: => NodeInformer) =
          visitor(n, cnt, depth, inf)
      }
  }

  type NodeT <: TraverserInnerNode
  trait TraverserInnerNode extends super.InnerNode { this: NodeT =>

    /** Instantiates an [[InnerNodeTraverser]] $EXTENDSTYPE `NodeT` $SETROOT. $TOSTART
      *  @param parameters $PARAMETERS
      */
    @inline final def innerNodeTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.innerNodeTraverser(this, parameters)

    /** Instantiates an [[OuterNodeTraverser]] $EXTENDSTYPE `N` $SETROOT. $TOSTART
      *  @param parameters $PARAMETERS
      */
    @inline final def outerNodeTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.outerNodeTraverser(this, parameters)

    /** Instantiates an [[InnerEdgeTraverser]] $EXTENDSTYPE `EdgeT` $SETROOT. $TOSTART
      *  @param parameters $PARAMETERS
      */
    @inline final def innerEdgeTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.innerEdgeTraverser(this, parameters)

    /** Instantiates an [[OuterEdgeTraverser]] $EXTENDSTYPE `E[N]` $SETROOT. $TOSTART
      *  @param parameters $PARAMETERS
      */
    @inline final def outerEdgeTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.outerEdgeTraverser(this, parameters)

    /** Instantiates an [[InnerElemTraverser]] $EXTENDSTYPE `InnerElem` $SETROOT. $TOSTART
      *  @param parameters $PARAMETERS
      */
    @inline final def innerElemTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.innerElemTraverser(this, parameters)

    /** Instantiates an [[OuterElemTraverser]] $EXTENDSTYPE `OuterElem` $SETROOT. $TOSTART
      *  @param parameters $PARAMETERS
      */
    @inline final def outerElemTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.outerElemTraverser(this, parameters)

    /** Instantiates an [[InnerNodeDownUpTraverser]] $EXTENDSTYPE `(Boolean, NodeT)` $SETROOT.
      *  $TOSTART
      *  @param parameters $PARAMETERS
      */
    @inline final def innerNodeDownUpTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.innerNodeDownUpTraverser(this, parameters)

    /** Instantiates an [[OuterNodeDownUpTraverser]] $EXTENDSTYPE `(Boolean, N)` $SETROOT.
      *  $TOSTART
      *  @param parameters $PARAMETERS
      */
    @inline final def outerNodeDownUpTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.outerNodeDownUpTraverser(this, parameters)
  }
  @transient object TraverserInnerNode {
    /* The n parameter should be of type NodeT but then Scaladoc doesn't show implicit members
     * as expected. So TraverserInnerNode is given instead with the drawback of a cast:(.
     */
    implicit def toDefaultTraverser(n: TraverserInnerNode): TraverserMethods[NodeT, InnerNodeTraverser] =
      innerNodeTraverser(n.asInstanceOf[NodeT])
  }

  /** Properties controlling the scope of traversals. */
  protected trait SubgraphProperties {

    /** $SUBGRAPHNODES */
    def subgraphNodes: NodeFilter

    /** $SUBGRAPHEDGES */
    def subgraphEdges: EdgeFilter
  }
  protected object SubgraphProperties {
    def apply[A](t: Traversable[A], nodeFilter: NodeFilter, edgeFilter: EdgeFilter) =
      new AbstractTraversable[A] with SubgraphProperties {
        def foreach[U](f: A => U): Unit = t foreach f
        def subgraphNodes               = nodeFilter
        def subgraphEdges               = edgeFilter
      }
  }

  /** Properties controlling traversals. */
  protected trait Properties extends SubgraphProperties {

    /** $ROOT*/
    def root: NodeT

    /** $PARAMETERS */
    def parameters: Parameters

    /** $ORD */
    def ordering: ElemOrdering

    /** $MAXWEIGHT */
    def maxWeight: Option[Weight] = None
  }

  /** [[Properties]] and methods for creating modified properties in a fluent-interface manner.
    *
    * @define UPDATED Creates a new [[FluentProperties]] based on `this` except for an updated
    */
  abstract protected class FluentProperties[+This <: FluentProperties[This]] {
    this: This with Properties =>

    protected def newTraverser: (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => This

    /** $UPDATED `parameters`. */
    final def withParameters(parameters: Parameters): This =
      if (this.parameters == parameters) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

    /** $UPDATED `subgraphNodes` and/or `subgraphEdges`. */
    final def withSubgraph(nodes: NodeFilter = anyNode, edges: EdgeFilter = anyEdge): This =
      if ((this.subgraphNodes eq nodes) &&
          (this.subgraphEdges eq edges)) this
      else newTraverser(root, parameters, nodes, edges, ordering, maxWeight)

    /** $UPDATED `ordering`. */
    final def withOrdering(ordering: ElemOrdering): This =
      if (this.ordering eq ordering) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

    /** $UPDATED `kind`. */
    final def withKind(kind: Kind): This =
      withParameters(parameters.withKind(kind))

    /** $UPDATED `direction`. Note that methods returning a Cycle or Path accept only `Successors`.*/
    final def withDirection(direction: Direction): This =
      withParameters(parameters.withDirection(direction))

    /** $UPDATED `maxDepth`. */
    final def withMaxDepth(maxDepth: Int): This =
      withParameters(parameters.withMaxDepth(maxDepth))

    /** $UPDATED `maxWeight`. */
    def withMaxWeight(maxWeight: Option[Weight]): This =
      if (this.maxWeight eq maxWeight) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

    /** $UPDATED `maxWeight` having the given `max` value and the given weight function. */
    final def withMaxWeight[W: Numeric](max: W, edgeWeight: EdgeT => W): This =
      withMaxWeight(Some(Weight(max, edgeWeight)))

    /** $UPDATED `maxWeight` having the given `max` and the default weight function returning `edge.weight`. */
    final def withMaxWeight(max: Long): This =
      withMaxWeight(Some(Weight(max)))

    final def toInnerElemTraverser(root: NodeT): InnerElemTraverser =
      innerElemTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)
  }

  /** Represents a component of `this` graph.
    *  Edges and bridges are computed lazily.
    *  Components will be instantiated by [[componentTraverser]] or [[strongComponentTraverser]].
    */
  abstract class Component protected extends Properties {

    def nodes: Set[NodeT]

    final lazy val (edges: Set[EdgeT], frontierEdges: Set[EdgeT]) = {
      val edges   = new ArrayBuffer[EdgeT](graphSize / 2)
      val bridges = new ArrayBuffer[EdgeT](if (mayHaveFrontierEdges) max(graphSize / 100, 16) else 0)
      for (n <- nodes) n.edges.withFilter(subgraphEdges) foreach { e =>
        if (nonBridge(e)) edges += e
        else bridges += e
      }
      (new EqSetFacade(edges), new EqSetFacade(bridges))
    }

    final def frontierEdges(that: Component): Set[EdgeT] =
      if (this.mayHaveFrontierEdges && that.mayHaveFrontierEdges) {
        // optimize calls of 'contains' because EqSetFacade's is O(N)
        def toEqSet(bridges: Set[EdgeT]) =
          (new EqHashSet[EdgeT](bridges.size) /: bridges)((eqSet, e) => eqSet += e)
        val (left, right) =
          if (this.frontierEdges.size > that.frontierEdges.size) (this.frontierEdges, toEqSet(that.frontierEdges))
          else (toEqSet(this.frontierEdges), that.frontierEdges)
        new EqSetFacade(left intersect right)
      } else Set.empty

    final def to[
        G[X, Y[X] <: EdgeLikeIn[X]] <: Graph[X, Y] with GraphLike[X, Y, G]
    ](factory: GraphCoreCompanion[G]): G[N, E] = thisGraph match {
      case g: Graph[N, E] =>
        factory.from(edges = edges.map(_.toOuter))(g.edgeT, factory.defaultConfig)
    }

    protected def mayHaveFrontierEdges: Boolean
    final private def nonBridge(e: EdgeT): Boolean = e.nodes forall nodes.contains

    protected def stringPrefix: String
    override def toString = s"$stringPrefix(${nodes mkString ", "})"
  }

  /** Controls the properties of graph traversals with no specific root and allows
    *  you to produce the (weakly) connected components by a traversal or
    *  call methods like `findCycle` that work component-wise.
    */
  abstract class ComponentTraverser protected
      extends FluentProperties[ComponentTraverser]
      with Properties
      with Traversable[Component] {

    def findCycle[U](implicit visitor: InnerElem => U = empty): Option[Cycle]

    /** See [[GraphTraversal#topologicalSort]]. */
    def topologicalSort[U](implicit visitor: InnerElem => U = empty): CycleNodeOrTopologicalOrder

    /** See [[GraphTraversal#topologicalSortByComponent]]. */
    def topologicalSortByComponent[U](
        implicit visitor: InnerElem => U = empty): Traversable[CycleNodeOrTopologicalOrder]
  }

  /** Creates a [[ComponentTraverser]] responsible for invoking graph traversal methods in all
    *  (weakly) connected components of this possibly disconnected graph.
    *
    * @param parameters $PARAMETERS
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    * @param maxWeight $MAXWEIGHT
    */
  def componentTraverser(parameters: Parameters = Parameters(),
                         subgraphNodes: NodeFilter = anyNode,
                         subgraphEdges: EdgeFilter = anyEdge,
                         ordering: ElemOrdering = NoOrdering,
                         maxWeight: Option[Weight] = None): ComponentTraverser

  /** Controls the properties of graph traversals with no specific root and allows
    *  you to produce the strongly connected components by a traversal.
    */
  abstract class StrongComponentTraverser protected
      extends FluentProperties[StrongComponentTraverser]
      with Properties
      with Traversable[Component]

  /** Creates a [[StrongComponentTraverser]].
    *
    * @param parameters $PARAMETERS
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    * @param maxWeight $MAXWEIGHT
    */
  def strongComponentTraverser(parameters: Parameters = Parameters(),
                               subgraphNodes: NodeFilter = anyNode,
                               subgraphEdges: EdgeFilter = anyEdge,
                               ordering: ElemOrdering = NoOrdering,
                               maxWeight: Option[Weight] = None): StrongComponentTraverser

  /** The `root`-related methods [[Traverser]] will inherit.
    *
    * @define SHORTESTPATH Finds the shortest path from `root` to `potentialSuccessor`
    *         $CONSIDERING The calculation is based on the weight of the edges on the path.
    *         Edges have a default weight of `1L` that can be overridden by custom edges.
    *         A weight function yielding any numeric type may also be passed to `shortestPathTo`.
    * @define POTENTIALSUCC The node the shortest path is to be found to.
    * @define SHORTESTPATHRET The shortest path to `potentialSuccessor` or `None` if either
    *         a. there exists no path to `potentialSuccessor` or
    *         a. there exists a path to `potentialSuccessor` but $DUETOSUBG
    * @define DUETOSUBG due to withSubgraph settings this path was out of scope.
    * @define VISITORDURING Function to be called for each inner node or inner edge visited during the
    */
  abstract protected class TraverserMethods[A, +This <: TraverserMethods[A, This]] extends FluentProperties[This] {
    this: This with Properties =>

    def root: NodeT

    protected def nodeVisitor[U](f: A => U): (NodeT) => U
    protected def edgeVisitor[U](f: A => U): (EdgeT) => U

    /** $UPDATED `root`. */
    final def withRoot(root: NodeT): This =
      if (this.root eq root) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

    protected def apply[U](pred: NodeFilter = noNode, visitor: A => U = empty): Option[NodeT]

    final protected def requireSuccessors[A](block: => A): A = {
      val direction = parameters.direction
      require(direction eq Successors, s"Found $direction but only $Successors will be accepted.")
      block
    }

    /** Finds a successor of `root` for which the predicate `pred` holds $CONSIDERING
      *  `root` itself does not count as a match. This is also true if it has a hook.
      *  If several successors holding `pred` exist any one of them may be returned.
      *
      * @param pred The predicate which must hold for the resulting node.
      * @param visitor $OPTVISITOR
      * @return A node with the predicate `pred` or `None` if either
      *         a. there is no node with `pred` or
      *         a. there exists no path to such a node or
      *         a. there exists a path to such a node but $DUETOSUBG
      */
    final def findSuccessor[U](pred: NodeFilter)(implicit visitor: A => U = empty): Option[NodeT] =
      apply(pred, visitor)

    /** Checks whether `potentialSuccessor` is a successor of this node $CONSIDERING
      *  Same as `isPredecessorOf`.
      *
      * @param potentialSuccessor The node which is potentially a successor of this node.
      * @param visitor $OPTVISITOR
      * @return `true` if a path exists from this node to `potentialSuccessor` and
      *         it had not to be excluded due to a `subgraph*` restriction.
      */
    @inline final def hasSuccessor[U](potentialSuccessor: NodeT)(implicit visitor: A => U = empty): Boolean =
      findSuccessor(_ eq potentialSuccessor)(visitor).isDefined

    /** Same as `hasSuccessor`. */
    @inline final def isPredecessorOf[U](potentialSuccessor: NodeT)(implicit visitor: A => U = empty): Boolean =
      hasSuccessor(potentialSuccessor)(visitor)

    /** Finds a predecessor of `root` for which the predicate `pred` holds $CONSIDERING
      * `root` itself does not count as a match. This is also true if it has a hook.
      * If several predecessors exist the algorithm selects the first of them found.
      *
      * @param pred The predicate which must hold true for the resulting node.
      * @param visitor $OPTVISITOR
      * @return A node with the predicate `pred` or `None` if either
      *         a. there is no node with `pred` or
      *         a. there exists no path from such a node to this node or
      *         a. there exists a path from such a node to `root` but $DUETOSUBG
      */
    final def findPredecessor[U](pred: NodeFilter)(implicit visitor: A => U = empty): Option[NodeT] =
      withParameters(parameters.withDirection(Predecessors))(pred, visitor)

    /** Checks whether `potentialPredecessor` is a predecessor of `root` $CONSIDERING
      *  Same as `isSuccessorOf`.
      *
      * @param potentialPredecessor The node which is potentially a predecessor of `root`.
      * @param visitor $OPTVISITOR
      * @return `true` if a path exists from `potentialPredecessor` to `root` and
      *         it had not to be excluded due to `subgraph` properties.
      */
    @inline final def hasPredecessor[U](potentialPredecessor: NodeT)(implicit visitor: A => U = empty): Boolean =
      findPredecessor(_ eq potentialPredecessor)(visitor).isDefined

    /** Same as `hasPredecessor`. */
    @inline final def isSuccessorOf[U](potentialPredecessor: NodeT)(implicit visitor: A => U = empty): Boolean =
      hasPredecessor(potentialPredecessor)(visitor)

    /** Finds a node connected with `root` by any number of edges with any direction
      *  for which the predicate `pred` holds $CONSIDERING
      *  For directed or mixed graphs the node to be found is weakly connected with this node.
      * `root` itself does not count as a match. This is also true if it has a hook.
      * If several connected nodes exist with `pred` the algorithm selects any one of these.
      *
      * @param pred The predicate which must hold true for the resulting node.
      * @param visitor $OPTVISITOR
      * @return A node with the predicate `pred` or `None` if either
      *         a. there is no node with `pred` or
      *         a. there exists no connection to such a node or
      *         a. there exists a connection to such a node but $DUETOSUBG
      */
    final def findConnected[U](pred: NodeFilter)(implicit visitor: A => U = empty): Option[NodeT] =
      withParameters(parameters.withDirection(AnyConnected))(pred, visitor)

    /** Checks whether `potentialConnected` is a node (not necessarily directly)
      *  connected with `root` by any number of edges with any direction $CONSIDERING
      *  For directed or mixed graphs it is satisfactory that `potentialConnected` is
      *  weakly connected with `root`.
      *
      * @param potentialConnected The node which is potentially connected with `root`.
      * @param visitor $OPTVISITOR
      * @return `true` if a path exists from this node to `potentialConnected` and
      *         it had not to be excluded due to `subgraph` properties.
      */
    @inline final def isConnectedWith[U](potentialConnected: NodeT)(implicit visitor: A => U = empty): Boolean =
      findConnected(_ eq potentialConnected)(visitor).isDefined

    /** Finds a path from `root` to a successor of `root` for which `pred` holds $CONSIDERING
      * `root` itself does not count as a match. This is also true if it has a hook.
      * If several successors exist the algorithm selects any one of these.
      *
      * @param pred The predicate which must hold true for the successor.
      * @param visitor $OPTVISITOR
      * @return A path to a node with the predicate `pred` or `None` if either
      *         a. there is no node with `pred` or
      *         a. there exists no path to such a node or
      *         a. there exists a path to such a node but $DUETOSUBG
      */
    def pathUntil[U](pred: NodeFilter)(implicit visitor: A => U = empty): Option[Path]

    /** Finds a path from `root` to `potentialSuccessor` $CONSIDERING
      *
      * @param potentialSuccessor The node a path is to be found to.
      * @param visitor $OPTVISITOR
      * @return A path to `potentialSuccessor` or `None` if either
      *         a. there is no node with `pred` or
      *         a. there exists no path to such a node
      */
    final def pathTo[U](potentialSuccessor: NodeT)(implicit visitor: A => U = empty): Option[Path] = requireSuccessors {
      if (potentialSuccessor eq root) Some(Path.zero(root))
      else pathUntil(_ eq potentialSuccessor)(visitor)
    }

    /** $SHORTESTPATH
      *
      * @param potentialSuccessor $POTENTIALSUCC
      * @param visitor $OPTVISITOR
      * @return $SHORTESTPATHRET
      */
    @inline final def shortestPathTo[U](potentialSuccessor: NodeT)(implicit visitor: A => U = empty): Option[Path] =
      shortestPathTo(potentialSuccessor, Edge.defaultWeight, visitor)

    /** $SHORTESTPATH
      *
      * @param potentialSuccessor $POTENTIALSUCC
      * @param weight Function to determine the weight of edges. If supplied, this function
      *        takes precedence over edge weights.
      * @return $SHORTESTPATHRET
      */
    @inline final def shortestPathTo[T: Numeric](potentialSuccessor: NodeT, weight: EdgeT => T): Option[Path] =
      shortestPathTo(potentialSuccessor, weight, empty)

    /** $SHORTESTPATH
      *
      * @param potentialSuccessor $POTENTIALSUCC
      * @param weight Function to determine the weight of edges. If supplied, this function
      *        takes precedence over edge weights.
      * @param visitor $OPTVISITOR
      * @return $SHORTESTPATHRET
      */
    def shortestPathTo[T: Numeric, U](potentialSuccessor: NodeT, weight: EdgeT => T, visitor: A => U): Option[Path]

    /** Finds a cycle starting the search at `root` $INTOACC, if any.
      *  The resulting cycle may start at any node connected with `this` node.
      *
      * @param visitor $OPTVISITOR
      * @return A cycle or `None` if either
      *  a. there exists no cycle in the component depicting by `root` or
      *  a. there exists a cycle in the component but $DUETOSUBG
      */
    def findCycle[U](implicit visitor: A => U = empty): Option[Cycle]

    /** Finds a cycle that contains `root` $INTOACC.
      * Irrespective of the current setting for `kind`, `DepthFirst` is used internally.
      *
      * @param visitor $OPTVISITOR
      * @return A cycle containing `root` or `None` if either
      *  a. there exists no cycle containing `root` or
      *  a. there exists such a cycle but $DUETOSUBG
      */
    def partOfCycle[U](implicit visitor: A => U = empty): Option[Cycle]

    /** Sorts the component designated by this node topologically.
      *  Only nodes connected with this node will be included in the resulting topological order.
      *  If the graph is known to be connected choose [[GraphTraversal#topologicalSort]] instead.
      *  $SEEFLUENT
      *
      *  @param ignorePredecessors If `true`, the topological sort will be partial in that it will only
      *                            include successors of `root`. `withSubgraph` restricts the successor nodes to
      *                            be included but not predecessors that will be excluded in total.
      *  @param visitor            $VISITORDURING sort.
      */
    def topologicalSort[U](ignorePredecessors: Boolean = false)(
        implicit visitor: InnerElem => U = empty): CycleNodeOrTopologicalOrder

    /** Determines the weak component that contains this node.
      *  $SEEFLUENT
      *
      *  @param visitor $VISITORDURING search.
      */
    def weakComponent[U](implicit visitor: A => U = empty): Component

    /** Finds all strongly connected components reachable from this node.
      *  $SEEFLUENT
      *
      *  @param visitor $VISITORDURING search.
      */
    def strongComponents[U](implicit visitor: A => U = empty): Iterable[Component]
  }

  /** Controls the properties of consecutive graph traversals starting at a root node.
    *  Provides methods to refine the properties and to invoke traversals.
    *  Instances will be created by [[innerNodeTraverser]] etc.
    */
  trait Traverser[A, +This <: Traverser[A, This]]
      extends TraverserMethods[A, This]
      with Properties
      with Traversable[A] {
    this: This =>

    def foreach[U](f: A => U): Unit =
      if (subgraphNodes(root))
        apply(noNode, f)

    /** Completes a traversal and creates a new connected graph populated with the
      *  elements visited.
      */
    final def toGraph: Graph[N, E] = thisGraph match {
      case g: Graph[N, E] =>
        val b = Graph.newBuilder(g.edgeT, Graph.defaultConfig)
        b += root
        val edgeTraverser = this match {
          case e: InnerEdgeTraverser => e
          case _                     => innerEdgeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
        }
        edgeTraverser foreach { e: EdgeT =>
          b += e
        }
        b.result
    }
  }

  /** Controls the properties of inner-node graph traversals. $TOSTART
    */
  abstract class InnerNodeTraverser extends Traverser[NodeT, InnerNodeTraverser]

  /** Creates a [[InnerNodeTraverser]] based on `scala.collection.Traversable[NodeT]`.
    *
    * @param root $ROOT
    * @param parameters $PARAMETERS
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    * @param maxWeight $MAXWEIGHT
    */
  def innerNodeTraverser(root: NodeT,
                         parameters: Parameters = Parameters(),
                         subgraphNodes: NodeFilter = anyNode,
                         subgraphEdges: EdgeFilter = anyEdge,
                         ordering: ElemOrdering = NoOrdering,
                         maxWeight: Option[Weight] = None): InnerNodeTraverser

  /** Controls the properties of outer-node graph traversals. $TOSTART
    */
  abstract class OuterNodeTraverser extends Traverser[N, OuterNodeTraverser]

  /** Creates a [[OuterNodeTraverser]] based on `scala.collection.Traversable[N]`.
    *
    * @param root $ROOT
    * @param parameters $PARAMETERS
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    * @param maxWeight $MAXWEIGHT
    */
  def outerNodeTraverser(root: NodeT,
                         parameters: Parameters = Parameters(),
                         subgraphNodes: NodeFilter = anyNode,
                         subgraphEdges: EdgeFilter = anyEdge,
                         ordering: ElemOrdering = NoOrdering,
                         maxWeight: Option[Weight] = None): OuterNodeTraverser

  /** Controls the properties of inner-edge graph traversals. $TOSTART
    */
  abstract class InnerEdgeTraverser extends Traverser[EdgeT, InnerEdgeTraverser]

  /** Creates a [[InnerEdgeTraverser]] based on `scala.collection.Traversable[EdgeT]`.
    *
    * @param root $ROOT
    * @param parameters $PARAMETERS
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    */
  def innerEdgeTraverser(root: NodeT,
                         parameters: Parameters = Parameters(),
                         subgraphNodes: NodeFilter = anyNode,
                         subgraphEdges: EdgeFilter = anyEdge,
                         ordering: ElemOrdering = NoOrdering,
                         maxWeight: Option[Weight] = None): InnerEdgeTraverser

  /** Controls the properties of outer-edge graph traversals. $TOSTART
    */
  abstract class OuterEdgeTraverser extends Traverser[E[N], OuterEdgeTraverser]

  /** Creates a [[OuterEdgeTraverser]] based on `scala.collection.Traversable[E[N]]`.
    *
    * @param root $ROOT
    * @param parameters $PARAMETERS
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    * @param maxWeight $MAXWEIGHT
    */
  def outerEdgeTraverser(root: NodeT,
                         parameters: Parameters = Parameters(),
                         subgraphNodes: NodeFilter = anyNode,
                         subgraphEdges: EdgeFilter = anyEdge,
                         ordering: ElemOrdering = NoOrdering,
                         maxWeight: Option[Weight] = None): OuterEdgeTraverser

  /** Controls the properties of inner-element graph traversals. $TOSTART
    */
  abstract protected class InnerElemTraverser extends Traverser[InnerElem, InnerElemTraverser]

  /** Creates a [[InnerElemTraverser]] based on `scala.collection.Traversable[InnerElem]`.
    *
    * @param root $ROOT
    * @param parameters $PARAMETERS
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    * @param maxWeight $MAXWEIGHT
    */
  def innerElemTraverser(root: NodeT,
                         parameters: Parameters = Parameters(),
                         subgraphNodes: NodeFilter = anyNode,
                         subgraphEdges: EdgeFilter = anyEdge,
                         ordering: ElemOrdering = NoOrdering,
                         maxWeight: Option[Weight] = None): InnerElemTraverser

  /** Controls the properties of outer-element graph traversals. $TOSTART
    */
  trait OuterElemTraverser extends Traverser[OuterElem[N, E], OuterElemTraverser]

  /** Creates a [[OuterElemTraverser]] based on `scala.collection.Traversable[OuterElem]`.
    *
    * @param root $ROOT
    * @param parameters $PARAMETERS
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    * @param maxWeight $MAXWEIGHT
    */
  def outerElemTraverser(root: NodeT,
                         parameters: Parameters = Parameters(),
                         subgraphNodes: NodeFilter = anyNode,
                         subgraphEdges: EdgeFilter = anyEdge,
                         ordering: ElemOrdering = NoOrdering,
                         maxWeight: Option[Weight] = None): OuterElemTraverser

  /** Controls the properties of inner-node down-up graph traversals. $TOSTART
    */
  abstract class InnerNodeDownUpTraverser extends Traverser[(Boolean, NodeT), InnerNodeDownUpTraverser]

  /** Creates a [[InnerNodeDownUpTraverser]] based on `scala.collection.Traversable[(Boolean, NodeT)]`
    *  $DOWNUPBOOLEAN
    *
    * @param root $ROOT
    * @param parameters $PARAMETERS A `kind` different from `DepthFirst` will be ignored.
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    * @param maxWeight $MAXWEIGHT
    */
  def innerNodeDownUpTraverser(root: NodeT,
                               parameters: Parameters = Parameters(),
                               subgraphNodes: NodeFilter = anyNode,
                               subgraphEdges: EdgeFilter = anyEdge,
                               ordering: ElemOrdering = NoOrdering,
                               maxWeight: Option[Weight] = None): InnerNodeDownUpTraverser

  /** Controls the properties of outer-node down-up graph traversals. $TOSTART
    */
  abstract class OuterNodeDownUpTraverser extends Traverser[(Boolean, N), OuterNodeDownUpTraverser]

  /** Creates a [[OuterNodeDownUpTraverser]] based on `scala.collection.Traversable[(Boolean, N)]`
    *  $DOWNUPBOOLEAN
    *
    * @param root $ROOT
    * @param parameters $PARAMETERS A `kind` different from `DepthFirst` will be ignored.
    * @param subgraphNodes $SUBGRAPHNODES
    * @param subgraphEdges $SUBGRAPHEDGES
    * @param ordering $ORD
    */
  def outerNodeDownUpTraverser(root: NodeT,
                               parameters: Parameters = Parameters(),
                               subgraphNodes: NodeFilter = anyNode,
                               subgraphEdges: EdgeFilter = anyEdge,
                               ordering: ElemOrdering = NoOrdering,
                               maxWeight: Option[Weight] = None): OuterNodeDownUpTraverser

  object Informer {
    type Depth = Int

    trait NodeElement {
      def node: NodeT
    }

    /** Extended node visitor informer for depth first searches.
      */
    trait DfsInformer extends NodeInformer {
      import DfsInformer._
      def stackIterator: DfsStack
      def pathIterator: DfsPath
    }
    object DfsInformer {
      case class Element protected[collection] (node: NodeT, depth: Depth, cumWeight: Double = 0) extends NodeElement
      object Element {
        def apply(node: NodeT) = new Element(node, 0)
      }
      type DfsStack = Iterator[Element]
      type DfsPath  = Iterator[Element]
      def unapply(inf: DfsInformer): Option[(DfsStack, DfsPath)] = Some(inf.stackIterator, inf.pathIterator)
    }

    case class CycleStackElem(node: NodeT, edges: Iterable[EdgeT]) extends NodeElement
    object CycleStackElem {
      def apply(node: NodeT) = new CycleStackElem(node, Nil)
    }

    /** Extended node visitor informer for cycle detecting.
      *  This informer always returns `0` for `depth`.
      */
    trait WgbInformer extends NodeInformer {
      import WgbInformer._
      def stackIterator: WgbStack
      def pathIterator: WgbPath
    }
    object WgbInformer {
      // exclude and multiEdges only needed for undirected edges
      case class Element protected[collection] (node: NodeT,
                                                predecessor: NodeT,
                                                exclude: Boolean,
                                                multiEdges: Iterable[EdgeT],
                                                cumWeight: Double = 0)
          extends NodeElement
      type WgbStack = Iterator[Element]
      type WgbPath  = Iterator[CycleStackElem]
      def unapply(inf: WgbInformer): Option[(WgbStack, WgbPath)] = Some(inf.stackIterator, inf.pathIterator)
    }

    /** Extended node visitor informer for breath first searches.
      */
    trait BfsInformer extends NodeInformer {
      import BfsInformer._
      def queueIterator: BfsQueue
    }
    object BfsInformer {
      type Element = DfsInformer.Element
      val Element = DfsInformer.Element
      type BfsQueue = Iterator[Element]
      def unapply(inf: BfsInformer): Option[BfsQueue] = Some(inf.queueIterator)
    }

    /** Extended node visitor informer for calculating shortest paths.
      *  This informer always returns `0` for `depth`.
      */
    abstract class DijkstraInformer[T: Numeric] extends NodeInformer {
      import DijkstraInformer._
      def queueIterator: DijkstraQueue[T]
      def costsIterator: DijkstraCosts[T]
    }
    object DijkstraInformer {
      case class Element[T: Numeric] protected[collection] (node: NodeT, cumWeight: T, depth: Depth) extends NodeElement
      type DijkstraQueue[T] = Iterator[Element[T]]
      type DijkstraCosts[T] = Iterator[(NodeT, T)]
      def unapply[T: Numeric](inf: DijkstraInformer[T]): Option[(DijkstraQueue[T], DijkstraCosts[T])] =
        Some(inf.queueIterator, inf.costsIterator)
    }

    /** Extended node visitor informer for Tarjan's algorithm.
      */
    abstract class TarjanInformer(val index: Int, val lowLink: Int) extends NodeInformer {
      import TarjanInformer._
      def stackIterator: TarjanStack
    }
    object TarjanInformer {
      type TarjanStack = Iterator[Element]
      case class Element protected[collection] (node: NodeT,
                                                depth: Depth,
                                                cumWeight: Double = 0,
                                                index: Int = 0,
                                                var lowLink: Int = 0)
          extends NodeElement
      def unapply[N](inf: TarjanInformer): Option[(Int, Int, TarjanStack)] =
        Some((inf.index, inf.lowLink, inf.stackIterator))
    }
  }
}

/** Contains traversal parameter definitions such as direction constants.
  *
  * @define KIND The kind of traversal including breadth-first and depth-fist search.
  * @define DIRECTION Determines which connected nodes the traversal has to follow.
  *         The default value is `Successors`.
  * @define MAXDEPTH A positive value to limit the number of layers for BFS respectively
  *         the number of consecutive child visits before siblings are visited for DFS.
  *         `0` - the default - indicates that the traversal should have
  *         an unlimited depth.
  * @author Peter Empen
  */
object GraphTraversal {

  /** Algebraic type to determine which connected nodes the traversal has to follow.
    *  The default value is `Successors`.
    *
    *  Note that methods returning a Cycle or Path accept only `Successors`.
    */
  sealed trait Direction

  /** Defines the traversal to follow successor nodes. */
  case object Successors extends Direction

  /** Defines the traversal to follow predecessor nodes. */
  case object Predecessors extends Direction

  /** Defines the traversal to follow successor and predecessor nodes alike. */
  case object AnyConnected extends Direction

  /** Marker trait for informers aimed at passing algorithmic-specific state
    *  to [[scalax.collection.GraphTraversal.ExtendedNodeVisitor]].
    *  Following informers are available:
    *  1. [[scalax.collection.GraphTraversal#Informer.BfsInformer]]
    *  1. [[scalax.collection.GraphTraversal#Informer.DfsInformer]]
    *  1. [[scalax.collection.GraphTraversal#Informer.WgbInformer]]
    *  1. [[scalax.collection.GraphTraversal#Informer.DijkstraInformer]]
    *  1. [[scalax.collection.GraphTraversal#Informer.TarjanInformer]].
    */
  trait NodeInformer
  object NodeInformer extends Serializable {
    def empty = new NodeInformer {}
  }

  /** Algebraic type for the kind of traversal. */
  trait Kind {

    /** Whether 'this' kind equals to BreadthFirst. */
    def isBsf = this eq BreadthFirst
  }

  /** Instructs the traverser to use a breadth first search
    *  (BSF, search layer-for-layer). */
  case object BreadthFirst extends Kind

  /** Instructs the traverser to use a depth first search (DFS). */
  case object DepthFirst extends Kind

  /** Parameters to control traversals.
    *
    * @param kind       $KIND
    * @param direction  $DIRECTION
    * @param maxDepth   $MAXDEPTH
    * @define UPDATED Creates a new `Parameters` based on `this` except for an updated
    */
  case class Parameters(kind: Kind = BreadthFirst, direction: Direction = Successors, maxDepth: Int = 0) {

    /** $UPDATED `kind`. */
    def withKind(kind: Kind): Parameters =
      if (this.kind eq kind) this else copy(kind = kind)

    /** $UPDATED `direction`. */
    def withDirection(direction: Direction): Parameters =
      if (this.direction == direction) this else copy(direction = direction)

    /** $UPDATED `maxDepth`. */
    def withMaxDepth(maxDepth: Int): Parameters =
      if (this.maxDepth == maxDepth) this else copy(maxDepth = maxDepth)
  }
  object Parameters {

    /** Default `Parameters`. */
    def apply = new Parameters()

    /** Creates `Parameters` of kind `BreadthFirst` with specific `direction` and `maxDepth`. */
    def Bfs(direction: Direction = Successors, maxDepth: Int = 0) =
      Parameters(direction = direction, maxDepth = maxDepth)

    /** Creates `Parameters` of kind `DepthFirst` with specific `direction` and `maxDepth`. */
    def Dfs(direction: Direction = Successors, maxDepth: Int = 0) =
      Parameters(kind = DepthFirst, direction = direction, maxDepth = maxDepth)
  }

  /** Implements an empty visitor based on a value.
    */
  object Visitor {
    final private val _empty: Any => Unit              = null
    @inline final def empty[A, U]: A => U              = _empty.asInstanceOf[A => U]
    @inline final def isEmpty[A, U](visitor: A => U)   = visitor eq _empty
    @inline final def isDefined[A, U](visitor: A => U) = visitor ne _empty
  }
}
