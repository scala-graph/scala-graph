package scalax.collection

import scala.language.{higherKinds, implicitConversions}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.math.min

import GraphPredef.{EdgeLikeIn, OuterElem, OuterEdge, OutParam, InnerNodeParam, InnerEdgeParam}
import mutable.EqHashMap

/** Graph-related functionality such as traversals, path finding, cycle detection etc.
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
 * @define INTOACC taking optional properties like
 *         subgraph restriction, ordering or maximum depth into account.
 * @define EXTNODEVISITOR Alternatively, an instance of `ExtendedNodeVisitor`
 *         may be passed to obtain additional state information such as the current
 *         depth. The concrete type of the last argument, the informer
 *         depends on the underlying implementation so you need to match against it.
 * @define ORD If a `NodeOrdering` or `EdgeOrdering` different from `noOrdering` is supplied
 *         neighbor nodes will visited during the traversal according to this ordering.
 * @define CONSIDERING considering all traversal properties passed to the traverser
 *         factory method like [[innerNodeTraverser]] or altered by any `with*` method.
 * @define OPTVISITOR An optional function that is applied for its side-effect to
 *         every element visited during graph traversal.
 * @define DUETOSUBG due to [[withSubgraph]] settings this path was out of scope.
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
 * 
 * @author Peter Empen
 */
trait GraphTraversal[N, E[X] <: EdgeLikeIn[X]] extends GraphBase[N,E] {
  thisGraph =>

  import GraphTraversal._
  import Visitor._

  /** Whether `this` graph is connected if it is undirected or
   *  weakly connected if it is directed.
   */
  def isConnected = nodes.headOption map { head =>
    head.innerNodeTraverser(
        Parameters(kind = DepthFirst, direction = AnyConnected)).size == nodes.size
  } getOrElse true
  
  /** Whether `this` graph has at least one cycle.
   */
  @inline final def isCyclic: Boolean = findCycle().isDefined

  /** Whether `this` graph has no cycle.
   */
  @inline final def isAcyclic: Boolean = ! isCyclic

  /** Finds a cycle in `this` graph and calls `visitor` for each inner element
   *  visited during the search.
   *  Use `componentTraverser` to pass non-default arguments.
   */
  @inline final def findCycle[U](
      implicit visitor: InnerElem => U = empty): Option[Cycle] =
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
  /** Default node filter letting traverse all nodes (non-filter). */
  @transient final val anyNode = (n: NodeT) => true
  /** Node predicate always returning `false`. */
  @transient final val noNode = (n: NodeT) => false
  /** Default edge filter letting path all edges (non-filter). */
  @transient final val anyEdge = (e: EdgeT) => true

  /** `true` if `f` is not equivalent to `anyNode`. */ 
  @inline final def isCustomNodeFilter(f: (NodeT) => Boolean)  = f ne anyNode   
  /** `true` if `f` is not equivalent to `anyEdge`. */ 
  @inline final def isCustomEdgeFilter(f: (EdgeT) => Boolean)  = f ne anyEdge   

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
<<<<<<< HEAD
  trait ExtendedNodeVisitor
      extends (NodeT => VisitorReturn)
      with ((NodeT, Int, Int, => NodeInformer) => VisitorReturn) 
      with Serializable {
=======
  trait ExtendedNodeVisitor[U]
      extends (NodeT => U)
      with ((NodeT, Int, Int, => NodeInformer) => U) {
>>>>>>> upstream/master
    def apply(node: NodeT) = apply(node, 0, 0, NodeInformer.empty)
  }
  object ExtendedNodeVisitor {
    /** Instantiates an extended node visitor based on 'visitor'.
     */
    def apply[N, E[X] <: EdgeLikeIn[X], U](
        visitor: (NodeT, Int, Int, => NodeInformer) => U) =
      new ExtendedNodeVisitor[U] {
        def apply(n: NodeT, cnt: Int, depth: Int, inf: => NodeInformer) =
          visitor(n, cnt, depth, inf)
      } 
  }  

  type NodeT <: TraverserInnerNode
  trait TraverserInnerNode extends super.InnerNode
  { this: NodeT =>
 
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
      withParameters(parameters.withKind(kind))

    /** $UPDATED `direction`. */
    final def withDirection(direction: Direction): This =
      withParameters(parameters.withDirection(direction))

    /** $UPDATED `maxDepth`. */
    final def withMaxDepth(maxDepth: Int): This =
      withParameters(parameters.withMaxDepth(maxDepth))

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
    
    def findCycle[U](implicit visitor: InnerElem => U = empty): Option[Cycle]
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

    protected def nodeVisitor[U](f: A => U): (NodeT) => U
    protected def edgeVisitor[U](f: A => U): (EdgeT) => U

    /** $UPDATED `root`. */
    final def withRoot(root: NodeT): This =
      if (this.root eq root) this
      else newTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
    
    protected def apply[U](pred:    (NodeT) => Boolean = noNode,
                           visitor: A => U             = empty): Option[NodeT]
    
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
    final def findSuccessor[U](pred: (NodeT) => Boolean)
                              (implicit visitor: A => U = empty): Option[NodeT] =
      apply(pred, visitor)

    /** Checks whether `potentialSuccessor` is a successor of this node $CONSIDERING
     *  Same as `isPredecessorOf`. 
     *
     * @param potentialSuccessor The node which is potentially a successor of this node. 
     * @param visitor $OPTVISITOR
     * @return `true` if a path exists from this node to `potentialSuccessor` and
     *         it had not to be excluded due to a `subgraph*` restriction.
     */
    @inline final def hasSuccessor[U](potentialSuccessor: NodeT)
                                     (implicit visitor: A => U = empty): Boolean =
      findSuccessor(_ eq potentialSuccessor)(visitor).isDefined

    /** Same as `hasSuccessor`. */
    @inline final def isPredecessorOf[U](potentialSuccessor: NodeT)
                                        (implicit visitor: A => U = empty): Boolean =
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
    final def findPredecessor[U](pred: (NodeT) => Boolean)
                                (implicit visitor: A => U = empty): Option[NodeT] =
      withParameters(parameters.withDirection(Predecessors))(pred, visitor)


    /** Checks whether `potentialPredecessor` is a predecessor of `root` $CONSIDERING
     *  Same as `isSuccessorOf`. 
     *
     * @param potentialPredecessor The node which is potentially a predecessor of `root`. 
     * @param visitor $OPTVISITOR
     * @return `true` if a path exists from `potentialPredecessor` to `root` and
     *         it had not to be excluded due to `subgraph` properties.
     */
    @inline final def hasPredecessor[U](potentialPredecessor: NodeT)
                                       (implicit visitor: A => U = empty): Boolean =
      findPredecessor(_ eq potentialPredecessor)(visitor).isDefined

    /** Same as `hasPredecessor`. */
    @inline final def isSuccessorOf[U](potentialPredecessor: NodeT)
                                      (implicit visitor: A => U = empty): Boolean =
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
    final def findConnected[U](pred: (NodeT) => Boolean)
                              (implicit visitor: A => U = empty): Option[NodeT] =
      withParameters(parameters.withDirection(AnyConnected))(pred, visitor)

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
    @inline final def isConnectedWith[U](potentialConnected: NodeT)
                                        (implicit visitor: A => U = empty): Boolean =
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
    def pathUntil[U](pred: (NodeT) => Boolean)
                    (implicit visitor: A => U = empty): Option[Path]

    /** Finds a path from `root` to `potentialSuccessor` $CONSIDERING
     *
     * @param potentialSuccessor The node a path is to be found to.
     * @param visitor $OPTVISITOR
     * @return A path to `potentialSuccessor` or `None` if either
     *         a. there is no node with `pred` or
     *         a. there exists no path to such a node
     */
    final def pathTo[U](potentialSuccessor: NodeT)
                       (implicit visitor: A => U = empty): Option[Path] =
      if (potentialSuccessor eq root) Some(Path.zero(root))
      else pathUntil(_ eq potentialSuccessor)(visitor)

    /** $SHORTESTPATH 
     *
     * @param potentialSuccessor $POTENTIALSUCC
     * @param visitor $OPTVISITOR
     * @return $SHORTESTPATHRET
     */
    @inline final
    def shortestPathTo[U](potentialSuccessor: NodeT)
                         (implicit visitor  : A => U = empty): Option[Path] =
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
      shortestPathTo(potentialSuccessor, weight, empty)

    /** $SHORTESTPATH
     *  
     * @param potentialSuccessor $POTENTIALSUCC
     * @param weight Function to determine the weight of edges. If supplied, this function
     *        takes precedence over edge weights. 
     * @param visitor $OPTVISITOR
     * @return $SHORTESTPATHRET 
     */
    def shortestPathTo[T:Numeric, U](potentialSuccessor: NodeT,
                                     weight            : EdgeT => T,
                                     visitor           : A => U): Option[Path]

    /** Finds a cycle starting the search at `root` $INTOACC, if any.
     *  The resulting cycle may start at any node connected with `this` node.
     * 
     * @param visitor $OPTVISITOR
     * @return A cycle or `None` if either
     *  a. there exists no cycle in the component depicting by `root` or
     *  a. there exists a cycle in the component but $DUETOSUBG
     */
    def findCycle[U](implicit visitor: A => U = empty): Option[Cycle]
  }

  /** Controls the properties of consecutive graph traversals starting at a root node.
   *  Provides methods to refine the properties and to invoke traversals.
   *  Instances will be created by [[innerNodeTraverser]] etc. 
   */
  trait Traverser[A, +This <: Traverser[A,This]]
      extends TraverserMethods[A,This]
         with Properties 
         with Traversable[A] {
    this: This =>
    
    def foreach[U](f: A => U): Unit =
      if (subgraphNodes(root))
        apply(noNode, f)

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
  abstract class InnerNodeTraverser extends Traverser[NodeT,InnerNodeTraverser]

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
  abstract class OuterNodeTraverser extends Traverser[N,OuterNodeTraverser]

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
  abstract class InnerEdgeTraverser extends Traverser[EdgeT,InnerEdgeTraverser]

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
  abstract class OuterEdgeTraverser extends Traverser[E[N],OuterEdgeTraverser]

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
  abstract class InnerNodeDownUpTraverser
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
  abstract class OuterNodeDownUpTraverser
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
   */
  sealed trait Direction  
  /** Defines the traversal to follow successor nodes. */
  object Successors   extends Direction 
  /** Defines the traversal to follow predecessor nodes. */
  object Predecessors extends Direction
  /** Defines the traversal to follow successor and predecessor nodes alike. */
  object AnyConnected extends Direction

  /** Marker trait for informers aimed at passing algorithmic-specific state
   *  to [[scalax.collection.GraphTraversal.ExtendedNodeVisitor]].
   *  Before calling an informer please match against one of 
   *  1. [[scalax.collection.GraphTraversalImpl.BfsInformer]]
   *  1. [[scalax.collection.GraphTraversalImpl.DfsInformer]]   
   *  1. [[scalax.collection.GraphTraversalImpl.WgbInformer]]
   *  1. [[scalax.collection.GraphTraversalImpl.DijkstraInformer]]
   *  or any other implementation that is currently not known.   
   */
  trait NodeInformer
  @transient object NodeInformer {
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

  /** Implements an empty visitor based on a value.
   */
  object Visitor {
    private final val _empty: Any => Unit = null
    @inline final def   empty  [A,U]: A => U = _empty.asInstanceOf[A => U]
    @inline final def isEmpty  [A,U](visitor: A => U) = visitor eq _empty   
    @inline final def isDefined[A,U](visitor: A => U) = visitor ne _empty   
  }
}
