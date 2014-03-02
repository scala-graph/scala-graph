package scalax.collection

import language.{higherKinds, implicitConversions, postfixOps}
import collection.mutable.ListBuffer

import GraphPredef.{EdgeLikeIn, OuterElem, OuterEdge, OutParam, InnerNodeParam, InnerEdgeParam}

/**
 * Defines traversal-related algorithmic interfaces.
 *
 * Graph traversal means to navigate from node to node based on incident edges.
 * Another kind of navigation is to iterate over nodes/edges based on the node-/edge-set.
 *
 * @define INTOACC taking optional filters and visitors into account.
 * @define VISITORS Node/edge visitor functions allow arbitrary user-defined computation
 *         during the traversal. 
 * @define DIRECTION Determines which connected nodes the traversal has to follow.
 *         The default value is `Successors`.
 * @define NODEFILTER Predicate to filter the nodes to be visited during traversal.
 *         The default value is `anyNode`, that is no filtering.
 *         A return of `true` signals that the traversal is to be canceled.
 * @define EDGEFILTER Predicate to filter the edges to be visited during traversal.
 *         The default value is `anyEdge` meaning that no filtering takes place.
 * @define NODEVISITOR Function to be called on visiting a node for the first time
 *         during a traversal. It can mutate the node or carry out any other side effect.
 *         The default value is the empty function `noNodeAction`. 
 * @define NODEUPVISITOR Function to be called on reaching an already visited node
 *         when moving up in the imaginary tree of a depth first search. Paired with
 *         `nodeVisitor` (the 'down-visitor'), this 'up-visitor' enables a stack-wise view of
 *         the traversed nodes. 
 *         The default value is the empty function `noNodeUpAction`.
 * @define EXTNODEVISITOR Alternatively, an instance of `ExtendedNodeVisitor`
 *         may be passed to obtain additional state information such as the current
 *         depth. The concrete type of the last argument, the informer
 *         depends on the underlying implementation so you need to match against it.
 * @define DFSINFORMER Concerning this method please match against
 *         [[scalax.collection.GraphTraversalImpl.DfsInformer]].   
 * @define BFSINFORMER Concerning this method please match against   
 *         [[scalax.collection.GraphTraversalImpl.BfsInformer]].
 * @define WGBINFORMER Concerning this method please match against   
 *         [[scalax.collection.GraphTraversalImpl.WgbInformer]].
 * @define DIJKSTRAINFORMER Concerning this method please match against   
 *         [[scalax.collection.GraphTraversalImpl.DijkstraInformer]].
 * @define ONEOFINFORMER Concerning this method please match against
 *         [[scalax.collection.GraphTraversalImpl.DfsInformer]] or
 *         [[scalax.collection.GraphTraversalImpl.BfsInformer]] depending on the
 *         `breadthFirst` argument.   
 * @define EDGEVISITOR Function to be called on visiting an edge.
 *         It can mutate the node or carry out any other side effect.
 *         The default value is the empty function `noEdgeAction`.
 * @define BREADTHFIRST If `true` the traversal is based on a breath first
 *         (BFS, layer-for-layer) search, otherwise on a depth first search (DFS).
 *         The default value is BFS.
 * @define MAXDEPTH A positive value limits the number of layers for BFS respectively
 *         the number of consecutive child visits before siblings are visited for DFS.
 *         `0` - the default - indicates that the traversal should have
 *         an unlimited depth meaning that it will be continued either until
 *         it's canceled by `nodeVisitor` or until all nodes have been visited.
 * @define ORD If a `NodeOrdering` or `EdgeOrdering` different from `noOrdering` is supplied
 *         neighbor nodes will be sorted during the traversal. Thus it is guaranteed that
 *         the smaller an element's ranking the sooner it will be processed. In case of
 *         `EdgeOrdering` it is guaranteed that the smaller an edge's ranking the sooner
 *         its relevant end(s) will be processed. 
 * @define RESULT the node found if any.
 * @define PRED The traversal stops at the first node except for `root` for which
 *         this predicate holds true and returns it.
 *         The default value `noNode` leads to a full traversal.
 *         
 * @define EXTENDSTYPE which extends `scala.collection.Traversable` with elements of type
 * @define SETROOT and sets its `root` to this node
 * @define TOSTART To start a traversal call any appropriate method inherited from `Traversable`
 *         on this instance.
 * @define ROOT The node where subsequent graph traversals start.
 * @define PARAMETERS The properties controlling subsequent traversals.
 * @define SUBGRAPHNODES Restricts subsequent graph traversals to visit only nodes holding this predicate.      
 * @define SUBGRAPHEDGES Restricts subsequent graph traversals to walk only along edges that hold this predicate.
 * @define DFSOF Controls `DepthFirst` graph traversals of
 * @define DOWNUPBOOLEAN where the `Boolean` parameter is `true` if the traversal takes place
 *         in downward and `false` if it takes place in upward direction.
 * @author Peter Empen
 */
trait GraphTraversal[N, E[X] <: EdgeLikeIn[X]] extends GraphBase[N,E] {
  thisGraph =>

  import GraphTraversal.VisitorReturn._
  import GraphTraversal._

  /** Whether `this` graph is connected if it is undirected or
   *  weakly connected if it is directed.
   */
  def isConnected = nodes.headOption map { head =>
    var cnt = 0
    head.traverseNodes(direction = AnyConnected, breadthFirst = false) { n =>
      cnt += 1
      Continue
    }
    cnt == nodes.size
  } getOrElse true
  
  /** Whether `this` graph has at least one cycle.
   */
  @inline final def isCyclic: Boolean = findCycle isDefined
  /**
   * Whether `this` graph has no cycle.
   */
  @inline final def isAcyclic: Boolean = ! isCyclic
  /**
   * Finds a cycle in `this` graph $INTOACC, if any.
   * 
   * @param nodeFilter $NODEFILTER
   * @param edgeFilter $EDGEFILTER
   * @param maxDepth   $MAXDEPTH
   * @param nodeVisitor $NODEVISITOR $EXTNODEVISITOR $WGBINFORMER
   * @param edgeVisitor $EDGEVISITOR
   * @param ordering   $ORD
   * @return A cycle or None if either
   *         a) there exists no cycle in `this` graph or
   *         b) there exists a cycle in `this` graph but due to the given
   *            filtering conditions or a `Cancel` return by a visitor this cycle
   *            had to be disregarded.
   */
  def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                edgeFilter : (EdgeT) => Boolean       = anyEdge,
                maxDepth   :  Int                     = 0,
                nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                ordering   : ElemOrdering             = noOrdering): Option[Cycle]
  /** Same as `findCycle(...)` with default arguments. */
  @inline final def findCycle: Option[Cycle] = findCycle()
  /**
   * Represents a path in this graph listing the nodes and connecting edges on it
   * with the following syntax:
   * 
   * `path ::= ''node'' { ''edge'' ''node'' }`
   * 
   * All nodes and edges on the path are distinct. A path contains at least
   * one node followed by any number of consecutive pairs of an edge and a node.
   * The first element is the start node, the second is an edge with its tail
   * being the start node and its head being the third element etc.   
   */
  trait Path extends Traversable[OutParam[N,E]]
  {
    override def stringPrefix = "Path"

    /** All nodes of this path in proper order. */
    def nodes: Traversable[NodeT]
    /** All edges of this path in proper order. */
    def edges: Traversable[EdgeT]

    /** The cumulated weight of all edges on this path. */
    def weight: Long = { var sum = 0L; edges foreach {sum += _.weight}; sum }  
    /** The number of edges on this path. */
    def length = nodes.size - 1
    def startNode: NodeT
    def endNode:   NodeT
    /**
     * Returns whether the contents of this path are valid with respect
     * to path semantics. This check is appropriate whenever there may be
     * any doubt about the correctness of the result of an algorithm. 
     */
    def isValid: Boolean
  }
  object Path {
    /** A path of zero length that is a single node. */
    def zero(node: NodeT) = new Path {
      private[this] val _node = node
      val nodes = List(_node)
      def edges = Nil
      def foreach[U](f: OutParam[N,E] => U): Unit = f(_node)
      def startNode = _node
      def endNode = _node
      def isValid = true
    }
  }
  /**
   * Whether all nodes are pairwise adjacent.
   * 
   * @return `true` if this graph is complete, `false` if this graph contains any
   * independent nodes. 
   */
  /**
   * Represents a cycle in this graph listing the nodes and connecting edges on it
   * with the following syntax:
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
    /**
     * Semantically compares `this` cycle with `that` cycle. While `==` returns `true`
     * only if the cycles contain the same elements in the same order, this comparison
     * returns also `true` if the elements of `that` cycle can be shifted and optionally
     * reversed such that their elements have the same order.
     * 
     * For instance, given
     * 
     * `c1 = Cycle(1-2-3-1)`, `c2 = Cycle(2-3-1-2)` and `c3 = Cycle(2-1-3-2)`
     * 
     * the following expressions hold:
     * 
     * `c1 != c2`, `c1 != c3` but `c1 sameAs c2` and `c1 sameAs c3`.  
     */
    def sameAs(that: GraphTraversal[N,E]#Cycle): Boolean
  }

  def isComplete = {
    val orderLessOne = order - 1
    nodes forall (_.diSuccessors.size == orderLessOne)
  }
  
  // Must be val since eq does not work for def.
  /** Default node filter letting path all nodes (non-filter). */
  @transient final val anyNode = (n: NodeT) => true
  /** Node predicate always returning `false`. */
  @transient final val noNode = (n: NodeT) => false
  /** Default edge filter letting path all edges (non-filter). */
  @transient final val anyEdge = (e: EdgeT) => true

  /** Default node visitor doing nothing (non-visitor). */
  @transient final val noNodeAction = (n: NodeT) => Continue
  /** Default node-up visitor doing nothing (non-visitor). */
  @transient final val noNodeUpAction = (n: NodeT) => {}
  /** Default edge visitor doing nothing (non-visitor). */
  @transient final val noEdgeAction = (e: EdgeT) => {}

  /** `true` if `filter` is not equivalent to `anyNode`. */ 
  @inline final def isCustomNodeFilter(filter: (NodeT) => Boolean)  = filter ne anyNode   
  /** `true` if `filter` is not equivalent to `anyEdge`. */ 
  @inline final def isCustomEdgeFilter(filter: (EdgeT) => Boolean)  = filter ne anyEdge   

  /** `true` if `visitor` is not equivalent to `noNodeAction`. */ 
  @inline final def isCustomNodeVisitor  (visitor: (NodeT) => VisitorReturn) = visitor ne noNodeAction   
  /** `true` if `visitor` is not equivalent to `noNodeUpAction`. */ 
  @inline final def isCustomNodeUpVisitor(visitor: (NodeT) => Unit         ) = visitor ne noNodeUpAction   
  /** `true` if `visitor` is not equivalent to `noEdgeAction`. */ 
  @inline final def isCustomEdgeVisitor  (visitor: (EdgeT) => Unit         ) = visitor ne noEdgeAction

  /** Template for extended node visitors.
   *  While the default node visitor of the type `Function1[NodeT, VisitorReturn]`
   *  supplies solely the inner node being visited extended node visitors
   *  supply the following traversal state information: 
   *  
   *  $ 1. the inner node currently visited as with a standard node visitor
   *  $ 2. the number of nodes visited so far and 
   *  $ 3. the current depth in terms of the underlying algorithm and 
   *  $ 4. a reference to a specific informer that may be pattern matched
   *       to collect even further data specific to the implementation.
   */
  trait ExtendedNodeVisitor
      extends (NodeT => VisitorReturn)
      with ((NodeT, Int, Int, => NodeInformer) => VisitorReturn) {
    def apply(node: NodeT) = apply(node, 0, 0, NodeInformer.empty)
  }
  object ExtendedNodeVisitor {
    /** Instantiates an extended node visitor based on 'visitor'.
     */
    def apply[N, E[X] <: EdgeLikeIn[X]](
        visitor: (NodeT, Int, Int, => NodeInformer) => VisitorReturn) =
      new ExtendedNodeVisitor {
        def apply(n: NodeT, cnt: Int, depth: Int, inf: => NodeInformer) =
          visitor(n, cnt, depth, inf)
      } 
  }  

  type NodeT <: TraverserInnerNode
  trait TraverserInnerNode extends super.InnerNode
  { this: NodeT =>

    /** Traverses this graph starting at this (root) node for side-effects allowing
     * 
     * a) to filter nodes and/or edges,
     * b) to carry out any side effect at visited nodes and/or edges and
     * c) to cancel the traversal at any node. 
     * 
     * @param direction $DIRECTION
     * @param nodeFilter $NODEFILTER $EXTNODEVISITOR $ONEOFINFORMER
     * @param edgeFilter $EDGEFILTER
     * @param breadthFirst $BREADTHFIRST
     * @param maxDepth     $MAXDEPTH
     * @param ordering   $ORD
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     */
    @deprecated("use innerElemTraverser or outerElemTraverser instead.", "1.8.0")
    final def traverse (direction  : Direction          = Successors,
                        nodeFilter : (NodeT) => Boolean = anyNode,
                        edgeFilter : (EdgeT) => Boolean = anyEdge,
                        breadthFirst:Boolean            = true,
                        maxDepth   : Int                = 0,
                        ordering   : ElemOrdering       = noOrdering)
                       (nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                        edgeVisitor: (EdgeT) => Unit           = noEdgeAction) =
      newTraversal(direction, nodeFilter,  edgeFilter, nodeVisitor, edgeVisitor, ordering).
          apply(this, noNode, breadthFirst, maxDepth)

    /**
     * Shortcut for calling `traverse` with a non-default `nodeVisitor` and
     * the default `edgeVisitor` allowing a `foreach`-like call syntax:
     * {{{
     * rootNode traverseNodes() {
     *   print("d" + _.degree)
     *   Continue
     * }
     * }}} 
     */
    @deprecated("use innerNodeTraverser or outerNodeTraverser instead.", "1.8.0") @inline final
    def traverseNodes(direction  : Direction          = Successors,
                      nodeFilter : (NodeT) => Boolean = anyNode,
                      edgeFilter : (EdgeT) => Boolean = anyEdge,
                      breadthFirst:Boolean            = true,
                      maxDepth   : Int                = 0,
                      ordering   : ElemOrdering       = noOrdering)
                     (nodeVisitor: (NodeT) => VisitorReturn) =
      traverse(direction, nodeFilter, edgeFilter, breadthFirst, maxDepth, ordering)(
               nodeVisitor = nodeVisitor)
    /**
     * Traverses this graph starting at this node for side-effects using dfs and applying
     * 'down' and 'up' node visitors to allow computations in a stack-wise manner:
     * {{{
       val root = "A"
       val g = Graph(root~>"B1", root~>"B2")
       val result = ListBuffer.empty[String]
       (g get root).traverseDownUp()(
         nodeDown = (n: g.NodeT) => {
           result. += (if (n == root) "{" else "(")
           result. += (n.value)
           Continue
         },
         nodeUp = (n: g.NodeT) =>
           result. += (if (n == root) "}" else ")")
       )
       ("" /: result)(_+_) // yields "{A(B1)(B2)}"
     * }}} 
     * @param direction  $DIRECTION
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param edgeVisitor $EDGEVISITOR
     * @param maxDepth   $MAXDEPTH
     * @param ordering   $ORD
     * @param nodeDown $NODEVISITOR $EXTNODEVISITOR $DFSINFORMER
     * @param nodeUp   $NODEUPVISITOR $EXTNODEVISITOR $DFSINFORMER
     */
    @inline final
    def traverseDownUp(direction  : Direction          = Successors,
                       nodeFilter : (NodeT) => Boolean = anyNode,
                       edgeFilter : (EdgeT) => Boolean = anyEdge,
                       edgeVisitor: (EdgeT) => Unit    = noEdgeAction,
                       maxDepth   : Int                = 0,
                       ordering   : ElemOrdering       = noOrdering)
                      (nodeDown: (NodeT) => VisitorReturn,
                       nodeUp  : (NodeT) => Unit) =
      newTraversal(direction, nodeFilter, edgeFilter, nodeDown, edgeVisitor, ordering).
        depthFirstSearch(this, maxDepth = maxDepth, nodeUpVisitor = nodeUp)
    /**
     * Shortcut for calling 'traverse' with a non-default `edgeVisitor`
     * but the default `nodeVisitor` allowing a `foreach`-like call syntax:
     * {{{
     * rootNode traverseEdges() {
     *   print( if(_.directed) "d" else "u" )
     *   Continue
     * }
     * }}} 
     */
    @deprecated("use innerEdgeTraverser or outerEdgeTraverser instead.", "1.8.0") @inline final
    def traverseEdges(direction  : Direction          = Successors,
                      nodeFilter : (NodeT) => Boolean = anyNode,
                      edgeFilter : (EdgeT) => Boolean = anyEdge,
                      breadthFirst:Boolean            = true,
                      maxDepth   : Int                = 0,
                      ordering   : ElemOrdering       = noOrdering)
                     (edgeVisitor: (EdgeT) => Unit       ) =
      traverse(direction, nodeFilter, edgeFilter, breadthFirst, maxDepth, ordering)(
               edgeVisitor = edgeVisitor)
 
    /** Instantiates an `InnerNodeTraverser` $EXTENDSTYPE `NodeT` $SETROOT. $TOSTART
     *  @param parameters $PARAMETERS
     */
    @inline final def innerNodeTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.innerNodeTraverser(this, parameters)

    /** Instantiates an `OuterNodeTraverser` $EXTENDSTYPE `N` $SETROOT. $TOSTART
     *  @param parameters $PARAMETERS   
     */
    @inline final def outerNodeTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.outerNodeTraverser(this, parameters)

    /** Instantiates an `InnerEdgeTraverser` $EXTENDSTYPE `EdgeT` $SETROOT. $TOSTART
     *  @param parameters $PARAMETERS
     */
    @inline final def innerEdgeTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.innerEdgeTraverser(this, parameters)

    /** Instantiates an `OuterEdgeTraverser` $EXTENDSTYPE `E[N]` $SETROOT. $TOSTART
     *  @param parameters $PARAMETERS   
     */
    @inline final def outerEdgeTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.outerEdgeTraverser(this, parameters)

    /** Instantiates an `InnerElemTraverser` $EXTENDSTYPE `InnerElem` $SETROOT. $TOSTART
     *  @param parameters $PARAMETERS
     */
    @inline final def innerElemTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.innerElemTraverser(this, parameters)

    /** Instantiates an `OuterElemTraverser` $EXTENDSTYPE `OuterElem` $SETROOT. $TOSTART
     *  @param parameters $PARAMETERS   
     */
    @inline final def outerElemTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.outerElemTraverser(this, parameters)
      
    /** Instantiates an `InnerNodeDownUpTraverser` $EXTENDSTYPE `(Boolean, NodeT)` $SETROOT.
     *  $TOSTART
     *  @param parameters $PARAMETERS   
     */
    @inline final def innerNodeDownUpTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.innerNodeDownUpTraverser(this, parameters)

    /** Instantiates an `OuterNodeDownUpTraverser` $EXTENDSTYPE `(Boolean, N)` $SETROOT.
     *  $TOSTART
     *  @param parameters $PARAMETERS   
     */
    @inline final def outerNodeDownUpTraverser(implicit parameters: Parameters = Parameters()) =
      thisGraph.outerNodeDownUpTraverser(this, parameters)
  }
  object TraverserInnerNode {
    implicit def toDefaultTraverser(n: NodeT): InnerNodeTraverser = innerNodeTraverser(n)
  }
  
  /** @define UPDATED Creates a new `Traverser` based on this except for an updated
   */
  protected abstract class Traverser[A, This <: Traverser[A,This]] extends Traversable[A] {
    this: This =>

    def root: NodeT
    def parameters: Parameters
    def subgraphNodes: (NodeT) => Boolean
    def subgraphEdges: (EdgeT) => Boolean
    def ordering: ElemOrdering
      
    protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering) => This

    protected def nodeVisitor[U](f: A => U): (NodeT) => VisitorReturn
    protected def edgeVisitor[U](f: A => U): (EdgeT) => Unit

    def foreach[U](f: A => U): Unit =
      root.traverse(
        parameters.direction, subgraphNodes, subgraphEdges,
        parameters.kind.isBsf, parameters.maxDepth, ordering)(nodeVisitor(f), edgeVisitor(f))

    /** $UPDATED `root`. */
    final def withRoot(root: NodeT): This =
      if (this.root eq root) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
    
    /** $UPDATED `parameters`. */
    final def withParameters(parameters: Parameters): This =
      if (this.parameters == parameters) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
    
    /** $UPDATED `subgraphNodes` and/or `subgraphEdges`. */
    final def withSubgraph(nodes: (NodeT) => Boolean = anyNode,
                           edges: (EdgeT) => Boolean = anyEdge): This =
      if ((this.subgraphNodes eq nodes) &&
          (this.subgraphEdges eq edges)) this
      else newTraverser(root, parameters, nodes, edges, ordering)
      
    /** $UPDATED `ordering`. */
    final def withOrdering(ordering: ElemOrdering): This =
      if (this.ordering eq ordering) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)

    /** $UPDATED `kind`. */
    final def withKind(kind: Kind): This =
      if (parameters.kind eq kind) this
      else withParameters(parameters.withKind(kind))

    /** $UPDATED `direction`. */
    final def withDirection(direction: Direction): This =
      if (parameters.direction == direction) this
      else withParameters(parameters.withDirection(direction))

    /** $UPDATED `maxDepth`. */
    final def withMaxDepth(maxDepth: Int): This =
      if (parameters.maxDepth == maxDepth) this
      else withParameters(parameters.withMaxDepth(maxDepth))

    /** Completes a traversal and creates a new graph populated with the elements visited.
     */
    final def toGraph: Graph[N, E] = thisGraph match {
      case g: Graph[N, E] =>
        val b = Graph.newBuilder(g.edgeT, Graph.defaultConfig)
        val edgeTraverser = this match {
          case t: InnerEdgeTraverser => t
          case t: Traverser[A, This] => innerEdgeTraverser(root, parameters,
            subgraphNodes, subgraphEdges, ordering)
        }
        edgeTraverser foreach { e: EdgeT => b += e }
        b.result
    }
    
    // `val` allows using reference equality
    @inline final protected val emptyVisitor: (A) => Unit = _ => Unit
    
    /** Finds a successor of `root` for which the predicate `pred` holds $INTOACC.
     *  $VISITORS
     *  `root` itself does not count as a match. This is also true if it has a hook.
     *  If several successors exist any one of them may be selected.
     * 
     * @param pred The predicate which must hold true for the resulting node.
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $DFSINFORMER
     * @return A node with the predicate `pred` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path to such a node at all
     *         c) there exists a path to such a node but due to
     *            user filtering or canceling the traversal this path had to be disregarded.
     */
    final def findSuccessor(pred: (NodeT) => Boolean)
                           (implicit visitor: A => Unit = emptyVisitor): Option[NodeT] =
      newTraversal(Successors, subgraphNodes, subgraphEdges, nodeVisitor(visitor),
                   edgeVisitor(visitor), ordering)(root, pred, parameters.kind.isBsf, 0)

    /** Checks whether `potentialSuccessor` is a successor of this node $INTOACC.
     *  $VISITORS
     *  Same as `isPredecessorOf`. 
     *
     * @param potentialSuccessor The node which is potentially a successor of this node. 
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $DFSINFORMER
     * @return `true` if a path exists from this node to `potentialSuccessor` and
     *         it had not to be excluded due to user filtering or canceling the traversal.
     */
    @inline final def hasSuccessor(potentialSuccessor: NodeT)
                                  (implicit visitor: A => Unit = emptyVisitor): Boolean =
      findSuccessor(_ eq potentialSuccessor)(visitor).isDefined

    /** Same as `hasSuccessor`. */
    @inline final def isPredecessorOf(potentialSuccessor: NodeT)
                                     (implicit visitor: A => Unit = emptyVisitor): Boolean =
      hasSuccessor(potentialSuccessor)(visitor)

    /** Finds a predecessor of this node for which the predicate `pred` holds $INTOACC.
     *  $VISITORS
     *
     * This node itself does not count as a match. This is also true if it has a hook.
     * If several predecessors exist the algorithm selects the first of them found.
     * 
     * @param pred The predicate which must hold true for the resulting node.
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $DFSINFORMER
     * @return A node with the predicate `pred` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path from such a node to this node at all or
     *         c) there exists a path from such a node to this node but due to
     *            user filtering or canceling the traversal this path had to be disregarded.
     */
    final def findPredecessor(pred: (NodeT) => Boolean)
                             (implicit visitor: A => Unit = emptyVisitor): Option[NodeT] =
      newTraversal(Predecessors, subgraphNodes, subgraphEdges, nodeVisitor(visitor),
                   edgeVisitor(visitor), ordering)(root, pred, parameters.kind.isBsf, 0)

    /** Checks whether `potentialPredecessor` is a predecessor of this node $INTOACC.
     *  $VISITORS
     *  Same as `isSuccessorOf`. 
     *
     * @param potentialPredecessor The node which is potentially a predecessor of this node. 
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $DFSINFORMER
     * @return `true` if a path exists from `potentialPredecessor` to this node and
     *         it had not to be excluded due to user filtering or canceling the traversal.
     */
    @inline final def hasPredecessor(potentialPredecessor: NodeT)
                                    (implicit visitor: A => Unit = emptyVisitor): Boolean =
      findPredecessor(_ eq potentialPredecessor)(visitor).isDefined

    /** Same as `hasPredecessor`. */
    @inline final def isSuccessorOf(potentialPredecessor: NodeT)
                                   (implicit visitor: A => Unit = emptyVisitor): Boolean =
      hasPredecessor(potentialPredecessor)(visitor)

    /** Finds a node (not necessarily directly) connected with this node
     *  for which the predicate `pred` holds $INTOACC.
     *  For directed or mixed graphs the node to be found is weekly connected with this node.
     *  $VISITORS
     *
     * This node itself does not count as a match. This is also true if it has a hook.
     * If several connected nodes exist with `pred` the algorithm selects the first
     * of them it founds.
     * 
     * @param pred The predicate which must hold true for the resulting node.
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $DFSINFORMER
     * @return A node with the predicate `pred` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no connection to such a node at all
     *         c) there exists a connection to such a node but due to
     *            user filtering or canceling the traversal this connection had to be disregarded.
     */
    final def findConnected(pred: (NodeT) => Boolean)
                           (implicit visitor: A => Unit = emptyVisitor): Option[NodeT] =
      newTraversal(AnyConnected, subgraphNodes, subgraphEdges, nodeVisitor(visitor),
                   edgeVisitor(visitor), ordering)(root, pred, parameters.kind.isBsf, 0)

    /** Checks whether `potentialConnected` is a node (not necessarily directly)
     *  connected with this node $INTOACC.
     *  For directed or mixed graphs it is satisfactory that `potentialConnected` is
     *  weekly connected with this node.
     *  $VISITORS
     *
     * @param potentialConnected The node which is potentially connected with this node. 
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $DFSINFORMER
     * @return `true` if a path exists from this node to `potentialConnected` and
     *         it had not to be excluded due to user filtering or canceling the traversal.
     */
    @inline final def isConnectedWith(potentialConnected: NodeT)
                                     (implicit visitor: A => Unit = emptyVisitor): Boolean =
      findConnected(_ eq potentialConnected)(visitor).isDefined

    /** Finds a path from this node to a successor of this node for which the predicate
     * `pred` holds $INTOACC.
     *
     * This node itself does not count as a match. This is also true if it has a hook.
     * If several successors exist the algorithm selects any first matching node.
     * 
     * @param pred The predicate which must hold true for the successor. 
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $DFSINFORMER
     * @return A path to a node with the predicate `pred` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path to such a node at all
     *         c) there exists a path to such a node but due to the given filter
     *            conditions this path had to be disregarded.
     */
    def pathUntil(pred: (NodeT) => Boolean)
                 (implicit visitor: A => Unit = emptyVisitor): Option[Path]

    /** Finds a path from this node to `potentialSuccessor`.
     *
     * @param potentialSuccessor The node a path is to be found to.
     * @return A path to `potentialSuccessor` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path to such a node at all
     */
    final def pathTo(potentialSuccessor: NodeT)
                    (implicit visitor: A => Unit = emptyVisitor): Option[Path] =
      if (potentialSuccessor eq root) Some(Path.zero(root))
      else pathUntil(_ eq potentialSuccessor)(visitor)

    /** Finds the shortest path from this node to `potentialSuccessor`.
     * 
     * The calculation is based on the weight of the edges on the path. As a default,
     * edges have a weight of 1 what can be overridden by custom edges. 
     *
     * @param potentialSuccessor The node the shortest path is to be found to.
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $DIJKSTRAINFORMER
     * @return The shortest path to `potentialSuccessor` or None if either
     *         a) there exists no path to `potentialSuccessor` or
     *         b) there exists a path to `potentialSuccessor` but due to the given
     *            filtering conditions this path had to be disregarded.
     */
    def shortestPathTo(potentialSuccessor: NodeT)
                      (implicit visitor: A => Unit = emptyVisitor): Option[Path]

    /** Finds a cycle starting the search at this node $INTOACC, if any.
     *  The resulting cycle may start at any node connected with `this` node.
     * 
     * @param visitor $NODEVISITOR $EXTNODEVISITOR $WGBINFORMER
     * @return A cycle or None if either
     *         a) there exists no cycle in the component `this` node belongs to or
     *         b) there exists a cycle in the component but due to the given
     *            filtering conditions or a `Cancel` return by a visitor this cycle
     *            had to be disregarded.
     */
    def findCycle(implicit visitor: A => Unit = emptyVisitor): Option[Cycle]
  }

  protected abstract class InnerNodeTraverser extends Traverser[NodeT,InnerNodeTraverser]

  /** Controls graph traversals of inner nodes based on `scala.collection.Traversable[NodeT]`.
   *  $TOSTART.
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def innerNodeTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): InnerNodeTraverser

  protected abstract class OuterNodeTraverser extends Traverser[N,OuterNodeTraverser]

  /** Controls graph traversals of outer nodes based on `scala.collection.Traversable[N]`.
   *  $TOSTART.
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def outerNodeTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): OuterNodeTraverser
    
  protected abstract class InnerEdgeTraverser extends Traverser[EdgeT,InnerEdgeTraverser]

  /** Controls graph traversals of inner edges based on `scala.collection.Traversable[EdgeT]`.
   *  $TOSTART.
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def innerEdgeTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): InnerEdgeTraverser
      
  protected abstract class OuterEdgeTraverser extends Traverser[E[N],OuterEdgeTraverser]

  /** Controls graph traversals of outer edges based on `scala.collection.Traversable[E[N]]`.
   *  $TOSTART.
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def outerEdgeTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): OuterEdgeTraverser 
 
  protected abstract class InnerElemTraverser extends Traverser[InnerElem,InnerElemTraverser]

  /** Controls graph traversals of inner nodes and edges based on `scala.collection.Traversable[InnerElem]`.
   *  $TOSTART.
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def innerElemTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): InnerElemTraverser
      
  trait OuterElemTraverser extends Traverser[OuterElem[N,E],OuterElemTraverser]

  /** Controls graph traversals of outer nodes and edges based on `scala.collection.Traversable[OuterElem]`.
   *  $TOSTART.
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def outerElemTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): OuterElemTraverser
      
  protected abstract class InnerNodeDownUpTraverser
      extends Traverser[(Boolean, NodeT),InnerNodeDownUpTraverser]

  /** $DFSOF inner nodes based on `scala.collection.Traversable[(Boolean, NodeT)]` $DOWNUPBOOLEAN
   *  $TOSTART.
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS A kind different from DepthFirst will be ignored.
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def innerNodeDownUpTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): InnerNodeDownUpTraverser
      
  protected abstract class OuterNodeDownUpTraverser
      extends Traverser[(Boolean, N),OuterNodeDownUpTraverser]

  /** $DFSOF outer nodes based on `scala.collection.Traversable[(Boolean, N)]` $DOWNUPBOOLEAN
   *  $TOSTART.
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS A kind different from DepthFirst will be ignored.
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def outerNodeDownUpTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): OuterNodeDownUpTraverser
      
  /** Abstract class for functional traversals.
   * 
   * In addition to the `traverse` methods defined for nodes, this concept supports
   * repeated traversals with constant direction, filters and visitors.
   * Call `newTraversal` to create an instance and call any subsequent traversals
   * on that instance.
   *     
   * @param direction $DIRECTION
   * @param nodeFilter $NODEFILTER
   * @param edgeFilter $EDGEFILTER
   * @param nodeVisitor $NODEVISITOR $EXTNODEVISITOR $ONEOFINFORMER
   * @param edgeVisitor $EDGEVISITOR
   * @param ordering   $ORD
   * 
   * @define FILTVIS function returning whether a specific node has already been visited
   *         during the current traversal
   * @define FILTREV whether to sort in reverse order. Only applicable when `ordering` is
   *         different from `noOrdering`.
   */
  abstract class Traversal(direction  : Direction,
                           nodeFilter : (NodeT) => Boolean,
                           edgeFilter : (EdgeT) => Boolean,
                           nodeVisitor: (NodeT) => VisitorReturn,
                           edgeVisitor: (EdgeT) => Unit,
                           ordering   : ElemOrdering)
  {
    final val noAction   = (n: NodeT) => {}
    final val notVisited = (n: NodeT) => false

    /** Computes the filtered direct successors of `node`. 
     * It also calls `edgeVisitor` but does '''not''' call `nodeVisitor`.
     * 
     * @param node the node the direct successors are to be calculated of.
     * @param isVisited $FILTVIS.
     * @param reverse $FILTREV
     */
    protected[collection]
    def filteredDiSuccessors(node     : NodeT,
                             isVisited: (NodeT) => Boolean,
                             reverse  : Boolean): Iterable[NodeT]
    /** Computes the filtered direct predecessors of `node`. 
     * It also calls `edgeVisitor` but does '''not''' call `nodeVisitor`.
     * 
     * @param node the node the direct predecessors are to be calculated of.
     * @param isVisited $FILTVIS.
     * @param reverse $FILTREV
     */
    protected[collection]
    def filteredDiPredecessors(node     : NodeT,
                               isVisited: (NodeT) => Boolean,
                               reverse  : Boolean): Iterable[NodeT]
    /** Computes the filtered neighbors of `node`. 
     * It also calls `edgeVisitor` but does '''not''' call `nodeVisitor`.
     * 
     * @param node the node the adjacent are to be calculated of.
     * @param isVisited $FILTVIS.
     * @param reverse $FILTREV
     */
    protected[collection]
    def filteredNeighbors(node     : NodeT,
                          isVisited: (NodeT) => Boolean,
                          reverse  : Boolean): Iterable[NodeT]
    /**
     * Traverses this graph from `root` for side-effects allowing
     * 
     * a) to filter nodes and/or edges,
     * b) to carry out any side effect at visited nodes and/or edges and
     * c) to cancel the traversal at any node. 
     * 
     * @param root         $ROOT
     * @param pred         $PRED
     * @param breadthFirst $BREADTHFIRST
     * @param maxDepth     $MAXDEPTH
     * @return $RESULT 
     */
    def apply(root        : NodeT,
              pred        : (NodeT) => Boolean = noNode,
              breadthFirst: Boolean            = true,
              maxDepth    : Int                = 0): Option[NodeT]
    /**
     * Starting at `root`, functionally traverses this graph up to `maxDepth` layers
     * using the depth first search algorithm and all filters, visitors etc.
     * passed to the encapsulating `Traversal` instance.
     * 
     * @param root $ROOT
     * @param pred $PRED
     * @param maxDepth $MAXDEPTH
     * @param nodeUpVisitor $NODEUPVISITOR
     *                      $EXTNODEVISITOR $DFSINFORMER
     * @return $RESULT 
     */
    def depthFirstSearch (root         : NodeT,
                          pred         : (NodeT) => Boolean = noNode,
                          maxDepth     : Int                = 0,
                          nodeUpVisitor: (NodeT) => Unit    = noNodeUpAction): Option[NodeT]
    /** Synonym for `depthFirstSearch` */
    @inline final def dfs(root         : NodeT,
                          pred         : (NodeT) => Boolean = noNode,
                          maxDepth     : Int                = 0,
                          nodeUpVisitor: (NodeT) => Unit    = noNodeUpAction) =
      depthFirstSearch(root, pred, maxDepth, nodeUpVisitor)
    /**
     * Starting at `root`, functionally traverses this graph up to `maxDepth` layers
     * using the breadth first search algorithm and all filters, visitors etc.
     * passed to the encapsulating `Traversal` instance.
     * 
     * @param root $ROOT
     * @param pred $PRED
     * @param maxDepth $MAXDEPTH
     * @return $RESULT 
     */
    def breadthFirstSearch(root    : NodeT,
                           pred    : (NodeT) => Boolean = noNode,
                           maxDepth: Int                = 0): Option[NodeT]
    /** Synonym for `breadthFirstSearch` */ 
    @inline final def bfs(root    : NodeT,
                          pred    : (NodeT) => Boolean = noNode,
                          maxDepth: Int                = 0) =
      breadthFirstSearch(root, pred, maxDepth)
  }
  /**
   * Creates a `Traversal` instance allowing subsequent traversals with
   * constant filters and visitors.
   * 
   * @param direction $DIRECTION
   * @param nodeFilter $NODEFILTER
   * @param edgeFilter $EDGEFILTER
   * @param nodeVisitor $NODEVISITOR $EXTNODEVISITOR $ONEOFINFORMER
   * @param edgeVisitor $EDGEVISITOR
   * @param ordering   $ORD
   */
  @deprecated("use one of the *Traverser classes instead.", "1.8.0") 
  def newTraversal(direction  : Direction                = Successors,
                   nodeFilter : (NodeT) => Boolean       = anyNode,
                   edgeFilter : (EdgeT) => Boolean       = anyEdge,
                   nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                   edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                   ordering   : ElemOrdering             = noOrdering): Traversal
}

/**
 * @define DIRECTION Determines which connected nodes the traversal has to follow.
 *         The default value is `Successors`.
 * @define MAXDEPTH A positive value limits the number of layers for BFS respectively
 *         the number of consecutive child visits before siblings are visited for DFS.
 *         `0` - the default - indicates that the traversal should have
 *         an unlimited depth meaning that it will be continued either until
 *         it's canceled by `nodeVisitor` or until all nodes have been visited.
 * @define UPDATED Creates a new `Traverser` based on this except for an updated
 */
object GraphTraversal {
  object VisitorReturn extends Enumeration {
    type VisitorReturn = Value
    val Continue, Cancel = Value
  }
  sealed trait Direction  
  object Successors   extends Direction 
  object Predecessors extends Direction
  object AnyConnected extends Direction
  
  trait NodeInformer
  @transient object NodeInformer {
    def empty = new NodeInformer {}
  }

  /** Kind of traversal. */
  trait Kind {
    def isBsf = this eq BreathFirst
  }
  case object BreathFirst extends Kind
  case object DepthFirst extends Kind

  /** @param direction  $DIRECTION
    * @param maxDepth   $MAXDEPTH
    * @define UPDATED Creates a new `Parameters` based on this except for an updated 
    */
  case class Parameters(kind     : Kind = BreathFirst,
                        direction: Direction = Successors,
                        maxDepth : Int = 0) {
    
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
    
    /** Creates `Parameters` of kind `BreathFirst` with specific `direction` and `maxDepth`. */
    def Bfs(direction: Direction = Successors, maxDepth : Int = 0) =
      Parameters(direction = direction, maxDepth = maxDepth)
    
    /** Creates `Parameters` of kind `DepthFirst` with specific `direction` and `maxDepth`. */
    def Dfs(direction: Direction = Successors, maxDepth : Int = 0) =
      Parameters(kind = DepthFirst, direction = direction, maxDepth = maxDepth)
  }
}
