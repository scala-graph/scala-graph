package scalax.collection

import scala.language.{higherKinds, implicitConversions, postfixOps}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.math.min

import GraphPredef.{EdgeLikeIn, OuterElem, OuterEdge, OutParam, InnerNodeParam, InnerEdgeParam}
import mutable.EqHashMap

/** Graph-related functionality such as traversal, path finding, cycle detection etc.
 *  All algorithms including breadth-first, depth-first, white-gray-black search and
 *  Dijkstra's algorithm are tail recursive.
 *  
 * Before starting a traversal a `Traverser` such as [[InnerNodeTraverser]] is instantiated
 * explicitly or implicitly. It holds settings like `maxDepth`, `subgraph` or `ordering`
 * providing a fine-grained control of the traversal. `Traverser`s also extend
 * `scala.collection.Traversable` meaning that you can process the visited nodes and edges
 * in a functional way.  
 *
 * @see [[http://www.scala-graph.org/guides/core-traversing]] 
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
 *         the traversed nodes. The default value is the empty function `noNodeUpAction`.
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
 * @define MAXDEPTH A positive value limiting the number of layers for Bfs respectively
 *         the number of consecutive child visits before siblings are visited for Dfs.
 *         `0`, the default value, indicates that the traversal should have
 *         an unlimited depth meaning that it will be continued until either
 *         it's canceled or all nodes have been visited.
 * @define KIND The kind of traversal including breadth-first and depth-fist search.
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
 * @define CONSIDERING considering all traversal properties passed to the traverser
 *         factory method like [[innerNodeTraverser]] or altered by any `with*` method.
 * @define OPTVISITOR An optional function that is applied for its side-effect to
 *         every element visited during graph traversal.
 * @define DUETOSUBG due to [[withSubgraph]] settings this path was out of scope.
 * @define EXTENDSTYPE which extends `scala.collection.Traversable` with elements of type
 * @define SETROOT and sets its `root` to this node
 * @define TOSTART To start a traversal call one of the graph traversal methods or 
 *         any appropriate method inherited from [[scala.collection.Traversable]] on this instance.
 * @define ROOT The node where subsequent graph traversals start.
 * @define PARAMETERS The properties controlling subsequent traversals.
 * @define SUBGRAPHNODES Restricts subsequent graph traversals to visit only nodes holding this predicate.      
 * @define SUBGRAPHEDGES Restricts subsequent graph traversals to walk only along edges that hold this predicate.
 * @define DOWNUPBOOLEAN where the `Boolean` parameter is `true` if the traversal takes place
 *         in downward and `false` if it takes place in upward direction.
 * @define PATHSYNTAX `::= ''node'' { ''edge'' ''node'' }`
 * @define WALKPATH A walk/path contains at least one node followed by any number of
 *         consecutive pairs of an edge and a node.
 *         The first element is the start node, the second is an edge with its source
 *         being the start node and its target being the third element etc.
 * @define SANECHECK This optional check is sane if there is reasonable doubt
 *         about the correctness of some algorithm results.
 * 
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

  /** Whether `this` graph has no cycle.
   */
  @inline final def isAcyclic: Boolean = ! isCyclic
  
  /** Finds a cycle in `this` graph $INTOACC, if any.
   * 
   * @param nodeFilter $NODEFILTER
   * @param edgeFilter $EDGEFILTER
   * @param maxDepth   $MAXDEPTH
   * @param nodeVisitor $NODEVISITOR $EXTNODEVISITOR $WGBINFORMER
   * @param edgeVisitor $EDGEVISITOR
   * @param ordering   $ORD
   * @return A cycle or `None` if either
   *         a. there exists no cycle in `this` graph or
   *         a. there exists a cycle in `this` graph but due to the given
   *            filtering conditions or a `Cancel` return by a visitor this cycle
   *            had to be disregarded.
   */
  @deprecated("Use componentTraverser instead.", "1.8.0")
  def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                edgeFilter : (EdgeT) => Boolean       = anyEdge,
                maxDepth   :  Int                     = 0,
                nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                ordering   : ElemOrdering             = noOrdering): Option[Cycle] =
    componentTraverser(Parameters(maxDepth = maxDepth), nodeFilter, edgeFilter, ordering).
      findCycle(_ match {
        case InnerNode(n) => nodeVisitor(n)
        case InnerEdge(e) => edgeVisitor(e)
      })
  
  /** Finds a cycle in `this` graph.
   *  Use `componentTraverser` to pass non-default arguments.
   */
  @inline final def findCycle: Option[Cycle] = componentTraverser().findCycle
  
  /* Drop the second signature and add `implicit` with a default empty visitor to this
   * as soon as the deprecated signature is removed.
   */
  /** Finds a cycle in `this` graph and calls `visitor` for each inner element
   *  visited during the search.
   *  Use `componentTraverser` to pass non-default arguments.
   */
  @inline final def findCycle(visitor: InnerElem => Unit) =
    componentTraverser().findCycle(visitor)

  @inline final protected def defaultPathSize: Int = min(256, nodes.size * 2)

  /** Represents a walk in this graph where
   * 
   * `walk` $PATHSYNTAX
   * 
   * $WALKPATH
   * 
   * @define CUMWEIGHT The cumulated weight of all edges on this path/walk.    
   */
  trait Walk extends Traversable[InnerElem]
  {
    override def stringPrefix = "Walk"

    /** All nodes on this path/walk in proper order. */
    def nodes: Traversable[NodeT]
    /** All edges of this path/walk in proper order. */
    def edges: Traversable[EdgeT]

    /** $CUMWEIGHT */
    final def weight: Long = (0L /: edges)((sum, edge) => sum + edge.weight)
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
    def endNode:   NodeT
    
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
     *  to this graph. $SANECHECK  
     */
    def isValid: Boolean = {
      val isValidNode = nodeValidator.apply _
      nodes.headOption filter isValidNode map { startNode =>
        val edges = this.edges.toIterator
        (nodes.head /: nodes.tail){ (prev: NodeT, n: NodeT) =>
          if (isValidNode(n) && edges.hasNext) {
            val e = edges.next
            if (! e.matches((x: NodeT) => x eq prev,
                            (x: NodeT) => x eq n   )) return false
            n
          } else return false
        }
        true
      } getOrElse false
    }

    protected trait NodeValidator {
      def apply(node: NodeT): Boolean
    }
    protected def nodeValidator: NodeValidator =
      new NodeValidator { 
        def apply(node: NodeT): Boolean = true
    }
  }
  protected trait ZeroWalk {
    this: Walk =>
    protected def single: NodeT
    val nodes = List(single)
    def edges = Nil
    def startNode = single
    def endNode = single
    override def isValid = true
  }
  object Walk {
    /** A walk of zero length that is a single node. */
    def zero(node: NodeT) =
      new Walk with ZeroWalk {
        protected final def single = node
      }
  }
  
  /** Represents a path in this graph where
   * 
   * `path` $PATHSYNTAX
   * 
   * Nodes and edges on the path are distinct. $WALKPATH
   */
  trait Path extends Walk
  {
    override def stringPrefix = "Path"

    /** Returns whether the nodes and edges on this path are valid with respect
     *  to this graph. $SANECHECK  
     */
    override def isValid: Boolean = super.isValid
    
    override protected def nodeValidator: NodeValidator =
      new NodeValidator { 
        private[this] val nodeSet = new EqHashMap[NodeT,Null](nodes.size)
        def apply(node: NodeT): Boolean = nodeSet.put(node, null).isEmpty
    }
  }
  object Path {
    /** A path of zero length that is a single node. */
    def zero(node: NodeT) =
      new Path with ZeroWalk {
        protected final def single = node
      }
  }
  
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

    /** Same as `sameAs` but also comparing this cycle with any `Traversable`.
     */
    final def sameElements(that: Traversable[_]): Boolean =
      this.size == that.size && {
        val thisList = to[List]
        // thisList.indexOf(that.head) may fail due to asymmetric equality
        val idx = thisList.indexWhere(_ == that.head)
        if (idx >= 0) {
          val thisDoubled = thisList ++ (thisList.tail)
          val thatList = that.to[List]
          (thisDoubled startsWith (thatList        , idx)) ||
          (thisDoubled startsWith (thatList.reverse, idx))
        }
        else false
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
    final def sameAs(that: GraphTraversal[N,E]#Cycle): Boolean =
      this == that || ( that match {
        case that: GraphTraversal[N,E]#Cycle => sameElements(that)
        case _ => false
      })

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

  /** `true` if `f` is not equivalent to `anyNode`. */ 
  @inline final def isCustomNodeFilter(f: (NodeT) => Boolean)  = f ne anyNode   
  /** `true` if `f` is not equivalent to `anyEdge`. */ 
  @inline final def isCustomEdgeFilter(f: (EdgeT) => Boolean)  = f ne anyEdge   

  /** `true` if `v` is not equivalent to `noNodeAction`. */ 
  @inline final def isCustomNodeVisitor  (v: (NodeT) => VisitorReturn) = v ne noNodeAction   
  /** `true` if `v` is not equivalent to `noNodeUpAction`. */ 
  @inline final def isCustomNodeUpVisitor(v: (NodeT) => Unit         ) = v ne noNodeUpAction   
  /** `true` if `v` is not equivalent to `noEdgeAction`. */ 
  @inline final def isCustomEdgeVisitor  (v: (EdgeT) => Unit         ) = v ne noEdgeAction

  /** Template for extended node visitors.
   *  While the default node visitor of the type `Function1[NodeT, VisitorReturn]`
   *  supplies solely the inner node being visited extended node visitors
   *  supply the following traversal state information: 
   *  1. the inner node currently visited as with a standard node visitor
   *  1. the number of nodes visited so far and 
   *  1. the current depth in terms of the underlying algorithm and 
   *  1. a reference to a specific informer that may be pattern matched
   *      to collect even further data specific to the implementation.
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
     *  a. to filter nodes and/or edges,
     *  a. to carry out any side effect at visited nodes and/or edges and
     *  a. to cancel the traversal at any node. 
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
    @deprecated("use innerNodeDownUpTraverser or outerNodeDownUpTraverser instead.", "1.8.0") @inline final
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
  object TraverserInnerNode {
    /* The n parameter should be of type NodeT but then Scaladoc doesn't show implicit members
     * as expected. So TraverserInnerNode is given instead with the drawback of a cast:(.
     */
    implicit def toDefaultTraverser(n: TraverserInnerNode)
        : TraverserMethods[NodeT,InnerNodeTraverser] = innerNodeTraverser(n.asInstanceOf[NodeT])
  }
  
  /** Properties controlling traversals.
   */
  protected trait Properties {
    /** $ROOT*/           def root: NodeT
    /** $PARAMETERS */    def parameters: Parameters
    /** $SUBGRAPHNODES */ def subgraphNodes: (NodeT) => Boolean
    /** $SUBGRAPHEDGES */ def subgraphEdges: (EdgeT) => Boolean
    /** $ORD */           def ordering: ElemOrdering      
  }
  
  /** [[Properties]] and methods for creating modified properties in a fluent-interface manner.
   *  
   * @define UPDATED Creates a new [[FluentProperties]] based on `this` except for an updated
   */
  protected abstract class FluentProperties[+This <: FluentProperties[This]] {
    this: This with Properties =>
      
    protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering) => This

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

    final def toInnerElemTraverser(root: NodeT): InnerElemTraverser =
      innerElemTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
  }
  
  /** Represents a component of `this` graph by a lazy implementation.
   *  Instances will be created by traversals based on [[componentTraverser]].  
   */
  protected abstract class Component extends Properties {
    def nodes: Set[NodeT]
    def edges: Set[EdgeT]
    def toGraph: Graph[N,E] =
      innerEdgeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering).toGraph
  }
  
  /** Controls the properties of consecutive graph traversals with no specific root.
   *  Provides methods to refine the properties and to invoke multiple traversals
   *  to cover all graph components. 
   */
  protected abstract class ComponentTraverser
      extends FluentProperties[ComponentTraverser]
         with Properties 
         with Traversable[Component] {
    
    @inline final protected def emptyVisitor(elem: InnerElem): Unit = ()

    def findCycle(implicit visitor: InnerElem => Unit = emptyVisitor): Option[Cycle]
  }
  
  /** Creates a [[ComponentTraverser]] responsible for invoking graph traversal methods
   *  that cover all components of this possibly disconnected graph.
   *    
   * @param parameters $PARAMETERS
   * @param subgraphNodes $SUBGRAPHNODES      
   * @param subgraphEdges $SUBGRAPHEDGES
   * @param ordering $ORD      
   */
  def componentTraverser(
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering): ComponentTraverser

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
   */
  protected abstract class TraverserMethods[A, +This <: TraverserMethods[A,This]]
      extends FluentProperties[This] {
    this: This with Properties =>

    def root: NodeT

    @inline final protected def chose[U]
        (f: NodeT => U, legacy: NodeT => VisitorReturn) = f match {
      case e: ExtendedNodeVisitor => e
      case _ => legacy
    }

    protected def nodeVisitor[U](f: A => U): (NodeT) => VisitorReturn
    protected def edgeVisitor[U](f: A => U): (EdgeT) => Unit

    /** $UPDATED `root`. */
    final def withRoot(root: NodeT): This =
      if (this.root eq root) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
    
    @inline final protected def emptyVisitor(a: A): Unit = ()
    
    /** Finds a successor of `root` for which the predicate `pred` holds $CONSIDERING
     *  `root` itself does not count as a match. This is also true if it has a hook.
     *  If several successors holding `pred` exist any one of them may be returden.
     * 
     * @param pred The predicate which must hold for the resulting node.
     * @param visitor $OPTVISITOR
     * @return A node with the predicate `pred` or `None` if either
     *         a. there is no node with `pred` or
     *         a. there exists no path to such a node or
     *         a. there exists a path to such a node but $DUETOSUBG
     */
    final def findSuccessor(pred: (NodeT) => Boolean)
                           (implicit visitor: A => Unit = emptyVisitor): Option[NodeT] =
      newTraversal(Successors, subgraphNodes, subgraphEdges, nodeVisitor(visitor),
                   edgeVisitor(visitor), ordering)(root, pred, parameters.kind.isBsf, 0)

    /** Checks whether `potentialSuccessor` is a successor of this node $CONSIDERING
     *  Same as `isPredecessorOf`. 
     *
     * @param potentialSuccessor The node which is potentially a successor of this node. 
     * @param visitor $OPTVISITOR
     * @return `true` if a path exists from this node to `potentialSuccessor` and
     *         it had not to be excluded due to a `subgraph*` restriction.
     */
    @inline final def hasSuccessor(potentialSuccessor: NodeT)
                                  (implicit visitor: A => Unit = emptyVisitor): Boolean =
      findSuccessor(_ eq potentialSuccessor)(visitor).isDefined

    /** Same as `hasSuccessor`. */
    @inline final def isPredecessorOf(potentialSuccessor: NodeT)
                                     (implicit visitor: A => Unit = emptyVisitor): Boolean =
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
    final def findPredecessor(pred: (NodeT) => Boolean)
                             (implicit visitor: A => Unit = emptyVisitor): Option[NodeT] =
      newTraversal(Predecessors, subgraphNodes, subgraphEdges, nodeVisitor(visitor),
                   edgeVisitor(visitor), ordering)(root, pred, parameters.kind.isBsf, 0)

    /** Checks whether `potentialPredecessor` is a predecessor of `root` $CONSIDERING
     *  Same as `isSuccessorOf`. 
     *
     * @param potentialPredecessor The node which is potentially a predecessor of `root`. 
     * @param visitor $OPTVISITOR
     * @return `true` if a path exists from `potentialPredecessor` to `root` and
     *         it had not to be excluded due to `subgraph` properties.
     */
    @inline final def hasPredecessor(potentialPredecessor: NodeT)
                                    (implicit visitor: A => Unit = emptyVisitor): Boolean =
      findPredecessor(_ eq potentialPredecessor)(visitor).isDefined

    /** Same as `hasPredecessor`. */
    @inline final def isSuccessorOf(potentialPredecessor: NodeT)
                                   (implicit visitor: A => Unit = emptyVisitor): Boolean =
      hasPredecessor(potentialPredecessor)(visitor)

    /** Finds a node connected with `root` by any number of edges with any direction
     *  for which the predicate `pred` holds $CONSIDERING
     *  For directed or mixed graphs the node to be found is weekly connected with this node.
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
    final def findConnected(pred: (NodeT) => Boolean)
                           (implicit visitor: A => Unit = emptyVisitor): Option[NodeT] =
      newTraversal(AnyConnected, subgraphNodes, subgraphEdges, nodeVisitor(visitor),
                   edgeVisitor(visitor), ordering)(root, pred, parameters.kind.isBsf, 0)

    /** Checks whether `potentialConnected` is a node (not necessarily directly)
     *  connected with `root` by any number of edges with any direction $CONSIDERING
     *  For directed or mixed graphs it is satisfactory that `potentialConnected` is
     *  weekly connected with `root`.
     *
     * @param potentialConnected The node which is potentially connected with `root`. 
     * @param visitor $OPTVISITOR
     * @return `true` if a path exists from this node to `potentialConnected` and
     *         it had not to be excluded due to `subgraph` properties.
     */
    @inline final def isConnectedWith(potentialConnected: NodeT)
                                     (implicit visitor: A => Unit = emptyVisitor): Boolean =
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
    def pathUntil(pred: (NodeT) => Boolean)
                 (implicit visitor: A => Unit = emptyVisitor): Option[Path]

    /** Finds a path from `root` to `potentialSuccessor` $CONSIDERING
     *
     * @param potentialSuccessor The node a path is to be found to.
     * @param visitor $OPTVISITOR
     * @return A path to `potentialSuccessor` or `None` if either
     *         a. there is no node with `pred` or
     *         a. there exists no path to such a node
     */
    final def pathTo(potentialSuccessor: NodeT)
                    (implicit visitor: A => Unit = emptyVisitor): Option[Path] =
      if (potentialSuccessor eq root) Some(Path.zero(root))
      else pathUntil(_ eq potentialSuccessor)(visitor)

    /** $SHORTESTPATH 
     *
     * @param potentialSuccessor $POTENTIALSUCC
     * @param visitor $OPTVISITOR
     * @return $SHORTESTPATHRET
     */
    @inline final
    def shortestPathTo(potentialSuccessor: NodeT)
                      (implicit visitor  : A => Unit = emptyVisitor): Option[Path] =
      shortestPathTo(potentialSuccessor, Edge.defaultWeight, visitor)

    /** $SHORTESTPATH
     *  
     * @param potentialSuccessor $POTENTIALSUCC
     * @param weight Function to determine the weight of edges. If supplied, this function
     *        takes precedence over edge weights. 
     * @return $SHORTESTPATHRET 
     */
    @inline final
    def shortestPathTo[T: Numeric](potentialSuccessor: NodeT,
                                   weight            : EdgeT => T): Option[Path] =
      shortestPathTo(potentialSuccessor, weight, emptyVisitor)

    /** $SHORTESTPATH
     *  
     * @param potentialSuccessor $POTENTIALSUCC
     * @param weight Function to determine the weight of edges. If supplied, this function
     *        takes precedence over edge weights. 
     * @param visitor $OPTVISITOR
     * @return $SHORTESTPATHRET 
     */
    def shortestPathTo[T: Numeric](potentialSuccessor: NodeT,
                                   weight            : EdgeT => T,
                                   visitor           : A => Unit): Option[Path]

    /** Finds a cycle starting the search at `root` $INTOACC, if any.
     *  The resulting cycle may start at any node connected with `this` node.
     * 
     * @param visitor $OPTVISITOR
     * @return A cycle or `None` if either
     *  a. there exists no cycle in the component depicting by `root` or
     *  a. there exists a cycle in the component but $DUETOSUBG
     */
    def findCycle(implicit visitor: A => Unit = emptyVisitor): Option[Cycle]
  }

  /** Controls the properties of consecutive graph traversals starting at a root node.
   *  Provides methods to refine the properties and to invoke traversals.
   *  Instances will be created by [[innerNodeTraverser]] etc. 
   */
  protected abstract class Traverser[A, +This <: Traverser[A,This]]
      extends TraverserMethods[A,This]
         with Properties 
         with Traversable[A] {
    this: This =>
    
    def foreach[U](f: A => U): Unit =
      if (subgraphNodes(root))
        newTraversal(parameters.direction, subgraphNodes, subgraphEdges,
                     nodeVisitor(f), edgeVisitor(f), ordering)(
                     root, noNode, parameters.kind.isBsf, parameters.maxDepth)

    /** Completes a traversal and creates a new connected graph populated with the
     *  elements visited.
     */
    final def toGraph: Graph[N,E] = thisGraph match {
      case g: Graph[N, E] =>
        val b = Graph.newBuilder(g.edgeT, Graph.defaultConfig)
        b += root
        val edgeTraverser = this match {
          case e: InnerEdgeTraverser => e
          case _ => innerEdgeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering) 
        }
        edgeTraverser foreach {
          e: EdgeT => b += e
        }
        b.result
    }    
  }

  /** Controls the properties of inner-node graph traversals. $TOSTART
   */
  protected abstract class InnerNodeTraverser extends Traverser[NodeT,InnerNodeTraverser]

  /** Creates a [[InnerNodeTraverser]] based on `scala.collection.Traversable[NodeT]`.
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

  /** Controls the properties of outer-node graph traversals. $TOSTART
   */
  protected abstract class OuterNodeTraverser extends Traverser[N,OuterNodeTraverser]

  /** Creates a [[OuterNodeTraverser]] based on `scala.collection.Traversable[N]`.
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
    
  /** Controls the properties of inner-edge graph traversals. $TOSTART
   */
  protected abstract class InnerEdgeTraverser extends Traverser[EdgeT,InnerEdgeTraverser]

  /** Creates a [[InnerEdgeTraverser]] based on `scala.collection.Traversable[EdgeT]`.
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
      
  /** Controls the properties of outer-edge graph traversals. $TOSTART
   */
  protected abstract class OuterEdgeTraverser extends Traverser[E[N],OuterEdgeTraverser]

  /** Creates a [[OuterEdgeTraverser]] based on `scala.collection.Traversable[E[N]]`.
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
 
  /** Controls the properties of inner-element graph traversals. $TOSTART
   */
  protected abstract class InnerElemTraverser extends Traverser[InnerElem,InnerElemTraverser]

  /** Creates a [[InnerElemTraverser]] based on `scala.collection.Traversable[InnerElem]`.
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
      
  /** Controls the properties of outer-element graph traversals. $TOSTART
   */
  trait OuterElemTraverser extends Traverser[OuterElem[N,E],OuterElemTraverser]

  /** Creates a [[OuterElemTraverser]] based on `scala.collection.Traversable[OuterElem]`.
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
      
  /** Controls the properties of inner-node down-up graph traversals. $TOSTART
   */
  protected abstract class InnerNodeDownUpTraverser
      extends Traverser[(Boolean, NodeT),InnerNodeDownUpTraverser]

  /** Creates a [[InnerNodeDownUpTraverser]] based on `scala.collection.Traversable[(Boolean, NodeT)]`
   *  $DOWNUPBOOLEAN
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS A `kind` different from `DepthFirst` will be ignored.
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
      
  /** Controls the properties of outer-node down-up graph traversals. $TOSTART
   */
  protected abstract class OuterNodeDownUpTraverser
      extends Traverser[(Boolean, N),OuterNodeDownUpTraverser]

  /** Creates a [[OuterNodeDownUpTraverser]] based on `scala.collection.Traversable[(Boolean, N)]`
   *  $DOWNUPBOOLEAN
   *    
   * @param root $ROOT
   * @param parameters $PARAMETERS A `kind` different from `DepthFirst` will be ignored.
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
  @deprecated("use one of Traverser factory methods instead.", "1.8.0") 
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
  @deprecated("use one of Traverser factory methods instead.", "1.8.0") 
  def newTraversal(direction  : Direction                = Successors,
                   nodeFilter : (NodeT) => Boolean       = anyNode,
                   edgeFilter : (EdgeT) => Boolean       = anyEdge,
                   nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                   edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                   ordering   : ElemOrdering             = noOrdering): Traversal
}

/** Contains traversal parameter definitions such as direction constants.
 *  
 * @define KIND The kind of traversal including breadth-first and depth-fist search.
 * @define DIRECTION Determines which connected nodes the traversal has to follow.
 *         The default value is `Successors`.
 * @define MAXDEPTH A positive value limits the number of layers for BFS respectively
 *         the number of consecutive child visits before siblings are visited for DFS.
 *         `0` - the default - indicates that the traversal should have
 *         an unlimited depth meaning that it will be continued either until
 *         it's canceled by `nodeVisitor` or until all nodes have been visited.
 * @author Peter Empen
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
    def isBsf = this eq BreadthFirst
  }
  case object BreadthFirst extends Kind
  case object DepthFirst extends Kind

  /** Parameters to control traversals.
   *
   * @param kind       $KIND
   * @param direction  $DIRECTION
   * @param maxDepth   $MAXDEPTH
   * @define UPDATED Creates a new `Parameters` based on `this` except for an updated 
   */
  case class Parameters(kind     : Kind = BreadthFirst,
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
    
    /** Creates `Parameters` of kind `BreadthFirst` with specific `direction` and `maxDepth`. */
    def Bfs(direction: Direction = Successors, maxDepth : Int = 0) =
      Parameters(direction = direction, maxDepth = maxDepth)
    
    /** Creates `Parameters` of kind `DepthFirst` with specific `direction` and `maxDepth`. */
    def Dfs(direction: Direction = Successors, maxDepth : Int = 0) =
      Parameters(kind = DepthFirst, direction = direction, maxDepth = maxDepth)
  }
}
