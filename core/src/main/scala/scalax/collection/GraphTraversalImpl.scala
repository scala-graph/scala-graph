package scalax.collection

import language.{higherKinds, implicitConversions}
import scala.annotation.{switch, tailrec}
import scala.collection.{AbstractTraversable, EqSetFacade}
import scala.collection.mutable.{ArrayBuffer, Buffer, ArrayStack => Stack, Map => MMap}

import GraphPredef.{EdgeLikeIn, Param, InParam, OutParam,
                    OuterNode, InnerNodeParam, OuterEdge, OuterElem, InnerEdgeParam}
import scalax.collection.GraphEdge.{DiEdgeLike, DiHyperEdgeLike, DiEdge, EdgeLike}
import mutable.{EqHashSet, EqHashMap}

/** Default implementation of the functionality defined by [[GraphTraversal]]
 *  except for algorithms that are placed in [[TraverserImpl]].
 *   
 *  @author Peter Empen
 */
trait GraphTraversalImpl[N, E[X] <: EdgeLikeIn[X]]
  extends GraphTraversal[N,E]
     with TraverserImpl[N,E]
     with State[N,E]
{ thisGraph: TraverserImpl[N,E] =>

  import GraphTraversalImpl._
  import GraphTraversal._
  import Visitor._
  import State._
  
  protected type CycleStackElem = (NodeT, Iterable[EdgeT])
  final protected def cycle(results: (Option[NodeT], Stack[CycleStackElem]),
                            edgeFilter: EdgeFilter): Option[Cycle] = {
    val (start, stack) = results
    start map { n: NodeT =>
      def toNode(elem: CycleStackElem) = elem._1
      def doWhile(elem: CycleStackElem): Boolean = elem._1 ne n
      val reverse: ReverseStackTraversable[CycleStackElem, NodeT] = {
          val enclosing: Array[Option[CycleStackElem]] = {
            val end = Some((n, Nil))
            Array[Option[CycleStackElem]](end, end)
          }
          new ReverseStackTraversable[CycleStackElem, NodeT](
              stack, toNode, Some(doWhile), enclosing)
      }
      if (thisGraph.isDirected) {
        new AnyEdgeLazyCycle  (reverse, edgeFilter)
      } else {
        new MultiEdgeLazyCycle(reverse, edgeFilter)
      }
    }
  }
    
  class WalkBuilder(override val start: NodeT,
                    sizeHint:           Int = defaultPathSize,
                    edgeSelector:       (NodeT, NodeT) => Option[EdgeT])
      extends super.WalkBuilder {
    self =>
      
    protected[this] var lastNode: Option[NodeT] = Some(start)
    private[this]   var lastEdge: Option[EdgeT] = None
    protected[this] val nodes = new ArrayBuffer[NodeT](sizeHint) += start
    protected[this] val edges = new ArrayBuffer[EdgeT](sizeHint)

    def add(node: NodeT): Boolean = 
      if( lastNode.fold[Boolean](
            // lastEdge, node
            ifEmpty = lastEdge.get.hasTarget(node))(
            // lastNode, node
            edgeSelector(_, node).fold(
              ifEmpty = false) {
              e => edges += e
                   true
            }
          )) {
        nodes += node
        lastNode = Some(node)
        lastEdge = None
        true
      } else false

    def add(edge: EdgeT): Boolean =
      if( lastEdge.fold[Boolean](
            // lastNode, edge
            ifEmpty = edge.hasSource(lastNode.get)
          ) {
            // lastEdge, edge
            lastEdge =>
              var sources, targets = Set.empty[NodeT]
              edge.    withSources(sources += _)
              lastEdge.withTargets(targets += _)
              val intersection = sources intersect targets
              if (intersection.isEmpty) false
              else
                select(intersection).fold[Boolean](
                  false){
                  n => nodes += n 
                       true
                }
          }) {
        edges += edge
        lastNode = None
        lastEdge = Some(edge)
        true
      } else false
      
    /* @param fromNodes Non-empty set of nodes to select from.
     */
    protected def select(fromNodes: Set[NodeT]): Option[NodeT] =
      Some(fromNodes.head)

    def clear: Unit = {
      nodes.clear; nodes += start
      edges.clear
      lastNode = Some(start)
      lastEdge = None
    }

    protected final def resultEdges = lastEdge.fold[IndexedSeq[EdgeT]] (
        ifEmpty = edges
      )(_ => edges.view(0, edges.size - 1))
    
    def result: Walk = new Walk {
      val nodes = self.nodes
      val edges = resultEdges
      val startNode = start
      val endNode = nodes(nodes.size - 1)
    }
  }
  
  def newWalkBuilder(
      start: NodeT)(
      implicit sizeHint: Int = defaultPathSize,
      edgeSelector:      (NodeT, NodeT) => Option[EdgeT]): WalkBuilder =
    new WalkBuilder(start, sizeHint, edgeSelector)
  
  class PathBuilder(override val start: NodeT,
                    sizeHint:           Int = defaultPathSize,
                    edgeSelector:       (NodeT, NodeT) => Option[EdgeT])
      extends WalkBuilder(start, sizeHint, edgeSelector)
         with super.PathBuilder {
    self =>
      
    private[this] val uniqueNodes = new EqHashSet[NodeT](sizeHint) += start
    
    override def add(node: NodeT): Boolean =
      if (uniqueNodes contains node) false
      else if (super.add(node)) {
        uniqueNodes += node
        true
      } else false
    
    override def add(edge: EdgeT): Boolean =
      if (lastNode.isDefined && edge.targets.forall(nodes contains _)) false
      else super.add(edge)
      
    override protected def select(fromNodes: Set[NodeT]): Option[NodeT] =
      fromNodes find (! uniqueNodes(_))
      
    override def clear: Unit = {
      super.clear
      uniqueNodes.clear
      uniqueNodes += start
    }

    override def result: Path = new Path {
      val nodes = self.nodes
      val edges = resultEdges
      val startNode = start
      val endNode = nodes(nodes.size - 1)
    }
  }
  
  def newPathBuilder(
      start: NodeT)(
      implicit sizeHint: Int = defaultPathSize,
      edgeSelector:      (NodeT, NodeT) => Option[EdgeT]): PathBuilder =
    new PathBuilder(start, sizeHint, edgeSelector)
  
  type NodeT <: InnerNodeTraversalImpl
  trait InnerNodeTraversalImpl extends TraverserInnerNode with InnerNodeState {
    this: NodeT =>
  }

  protected class ComponentImpl(
      override val root         : NodeT, 
      override val parameters   : Parameters,
      override val subgraphNodes: NodeFilter,
      override val subgraphEdges: EdgeFilter,
      override val ordering     : ElemOrdering,
      override val nodes        : Set[NodeT])
      extends Component {
    
    lazy val edges: Set[EdgeT] = {
      val edges = new ArrayBuffer[EdgeT](nodes.size * 2)
      for (n <- nodes) n.edges foreach (edges += _)
      new EqSetFacade(edges)
    }
    
    override def toString = s"Component(${nodes mkString ","})"
  }

  final protected def expectedMaxNodes(divisor: Int, min: Int = 128): Int = {
    val o = order
    if (o == 0) 1
    else math.min(o, math.max(o / divisor, min))
  }

  /** Calculates in-degrees of nodes spanned by `traversable`.
   *  @param traversable supplies the nodes for which the degree is to be calculated
   *  @param maybeHandle to be used to mark visited nodes
   *  @param includeAnyway include this node in the resulting list of nodes without predecessors
   *         irrespective of its in degree
   *  @param includeInDegree optionally filters predecessor nodes when calculation the in degree
   *  @return pair of
   *          a. nodes without predecessors in the component spanned by `traverser`
   *          a. map of visited nodes to their in degrees
   */
  protected final def forInDegrees(
      traversable: Traversable[NodeT] with SubgraphProperties,
      maybeHandle: Option[Handle] = None, 
      includeAnyway: Option[NodeT] = None,
      includeInDegree: NodeFilter = anyNode,
      fillInDegrees: Boolean = true): (Buffer[NodeT], MMap[NodeT,Int]) = {
    
    val nodesWithoutPredecessor = new ArrayBuffer[NodeT](expectedMaxNodes(1000))
    val nodeInDegrees = new EqHashMap[NodeT,Int](if (fillInDegrees) order else 0)
    def nodeFilter(n: NodeT) : Boolean = traversable.subgraphNodes(n) && includeInDegree(n)
    traversable foreach { n =>
      maybeHandle foreach (implicit h => n.visited = true)
      val inDegree = n.inDegree(nodeFilter)
      if (fillInDegrees) nodeInDegrees put (n, inDegree)
      if (inDegree == 0 || (n eq includeAnyway.orNull)) nodesWithoutPredecessor += n
    }
    (nodesWithoutPredecessor, nodeInDegrees)
  }
  
  protected case class ComponentTraverser(
      override val root         : NodeT, 
      override val parameters   : Parameters,
      override val subgraphNodes: NodeFilter,
      override val subgraphEdges: EdgeFilter,
      override val ordering     : ElemOrdering,
      override val maxWeight    : Option[Weight])
      extends super.ComponentTraverser {
    
    final protected def newTraverser:
        (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => ComponentTraverser = copy

    final private def innerElemTraverser =
      InnerElemTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)

    protected lazy val components: Iterable[ComponentImpl] = {
      val traverser = InnerNodeTraverser(
          root, parameters withDirection AnyConnected, subgraphNodes, subgraphEdges, ordering)
      withHandle() {  implicit visitedHandle =>
        for (node <- nodes if ! node.visited && subgraphNodes(node)) yield {
          val componentNodes = new ArrayBuffer[NodeT](expectedMaxNodes(6))
          traverser.withRoot(node) foreach { n =>
            n.visited = true
            componentNodes += n
          }
          new ComponentImpl(node, parameters, subgraphNodes, subgraphEdges, ordering,
              new EqSetFacade(componentNodes))
        }
      }
    }
    
    def foreach[U](f: Component => U): Unit = components foreach f
    
    def findCycle[U](implicit visitor: InnerElem => U = empty): Option[Cycle] =
      if (order == 0) None
      else {
        val traverser = innerElemTraverser
        withHandles(2) { handles =>
          implicit val visitedHandle = handles(0)
          for (node <- nodes if ! node.visited && subgraphNodes(node)) {
            val res = traverser.withRoot(node).Runner(noNode, visitor).dfsWGB(handles)
            if (res._1.isDefined)
              return cycle(res, subgraphEdges)
          }
        }
        None
      }

    final def topologicalSort[U](implicit visitor: InnerElem => U = empty): CycleNodeOrTopologicalOrder = {
      val (nodesWithoutPredecessor, inDegrees) = forInDegrees(SubgraphProperties(nodes, subgraphNodes, subgraphEdges))
      innerElemTraverser.Runner(noNode, visitor).topologicalSort(nodesWithoutPredecessor, inDegrees)
    }
    
    final def topologicalSortByComponent[U](implicit visitor: InnerElem => U = empty): Traversable[CycleNodeOrTopologicalOrder] =
      if (order == 0) Nil
      else {
        val topoRunner = innerElemTraverser.Runner(noNode, visitor)
        val forStartNodes = innerNodeTraverser(root, Parameters.Dfs(AnyConnected))
        withHandles(2) { handles: Array[Handle] =>
          val (startNodesHandle, topoHandle) = (Some(handles(0)), Some(handles(1)))
          implicit val handle = startNodesHandle.get
          for (node <- nodes if ! node.visited && subgraphNodes(node)) yield {
            val (startNodes, inDegrees) = forInDegrees(
                forStartNodes.withRoot(node), startNodesHandle, includeInDegree = subgraphNodes)
            topoRunner.topologicalSort(startNodes, inDegrees, topoHandle)
          }
        }
      }
  }
  
  def componentTraverser(
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    ComponentTraverser(null.asInstanceOf[NodeT], parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)
  
  protected case class InnerNodeTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters     = Parameters(),
      override val subgraphNodes: NodeFilter     = anyNode,
      override val subgraphEdges: EdgeFilter     = anyEdge,
      override val ordering     : ElemOrdering   = NoOrdering,
      override val maxWeight    : Option[Weight] = None)
      extends super.InnerNodeTraverser
         with Impl[NodeT,InnerNodeTraverser] {
    
    final protected def newTraverser:
      (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => InnerNodeTraverser = copy

    final protected def nodeVisitor[U](f: NodeT => U): (NodeT) => U = f
    final protected def edgeVisitor[U](f: NodeT => U): (EdgeT) => U = empty
  }
  
  def innerNodeTraverser(
      root         : NodeT,
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    InnerNodeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class OuterNodeTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters     = Parameters(),
      override val subgraphNodes: NodeFilter     = anyNode,
      override val subgraphEdges: EdgeFilter     = anyEdge,
      override val ordering     : ElemOrdering   = NoOrdering,
      override val maxWeight    : Option[Weight] = None)
      extends super.OuterNodeTraverser
         with Impl[N,OuterNodeTraverser] {
    
    final protected def newTraverser:
      (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => OuterNodeTraverser = copy

    final protected def nodeVisitor[U](f: N => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(n.value) else empty

    final protected def edgeVisitor[U](f: N => U): (EdgeT) => U = empty
  }

  def outerNodeTraverser(
      root         : NodeT,
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    OuterNodeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)
    
  protected case class InnerEdgeTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters     = Parameters(),
      override val subgraphNodes: NodeFilter     = anyNode,
      override val subgraphEdges: EdgeFilter     = anyEdge,
      override val ordering     : ElemOrdering   = NoOrdering,
      override val maxWeight    : Option[Weight] = None)
      extends super.InnerEdgeTraverser
         with Impl[EdgeT,InnerEdgeTraverser] {
    
    final protected def newTraverser:
    (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => InnerEdgeTraverser = copy

    final protected def nodeVisitor[U](f: EdgeT => U): (NodeT) => U = empty
    final protected def edgeVisitor[U](f: EdgeT => U): (EdgeT) => U =
      if (isDefined(f)) (e: EdgeT) => f(e) else empty
  }
  
  def innerEdgeTraverser(
      root         : NodeT,
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    InnerEdgeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)
  
  protected case class OuterEdgeTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters     = Parameters(),
      override val subgraphNodes: NodeFilter     = anyNode,
      override val subgraphEdges: EdgeFilter     = anyEdge,
      override val ordering     : ElemOrdering   = NoOrdering,
      override val maxWeight    : Option[Weight] = None)
      extends super.OuterEdgeTraverser
         with Impl[E[N],OuterEdgeTraverser] {
    
    final protected def newTraverser:
      (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => OuterEdgeTraverser = copy

    final protected def nodeVisitor[U](f: E[N] => U): (NodeT) => U = empty
    final protected def edgeVisitor[U](f: E[N] => U): (EdgeT) => U =
      if (isDefined(f)) (e: EdgeT) => f(e.toOuter) else empty
  }
  
  def outerEdgeTraverser(
      root         : NodeT,
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    OuterEdgeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)
  
  protected case class InnerElemTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters     = Parameters(),
      override val subgraphNodes: NodeFilter     = anyNode,
      override val subgraphEdges: EdgeFilter     = anyEdge,
      override val ordering     : ElemOrdering   = NoOrdering,
      override val maxWeight    : Option[Weight] = None)
      extends super.InnerElemTraverser
         with Impl[InnerElem,InnerElemTraverser] {
    
    final protected def newTraverser:
      (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => InnerElemTraverser = copy
        
    final protected def nodeVisitor[U](f: InnerElem => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(n) else f
    final protected def edgeVisitor[U](f: InnerElem => U): (EdgeT) => U =
      if (isDefined(f)) (e: EdgeT) => f(e) else f
  }
  
  def innerElemTraverser(
      root         : NodeT,
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    InnerElemTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class OuterElemTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters     = Parameters(),
      override val subgraphNodes: NodeFilter     = anyNode,
      override val subgraphEdges: EdgeFilter     = anyEdge,
      override val ordering     : ElemOrdering   = NoOrdering,
      override val maxWeight    : Option[Weight] = None)
      extends super.OuterElemTraverser
         with Impl[OuterElem[N,E],OuterElemTraverser] {
    
    final protected def newTraverser:
      (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => OuterElemTraverser = copy

    final protected def nodeVisitor[U](f: OuterElem[N,E] => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(n.value)
      else empty

    final protected def edgeVisitor[U](f: OuterElem[N,E] => U): (EdgeT) => U =
      if (isDefined(f)) (e: EdgeT) => f(e.toOuter.asInstanceOf[OuterEdge[N,E]])
      else empty
  }

  def outerElemTraverser(
      root         : NodeT,
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    OuterElemTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected trait DownUpTraverser[A, +This <: DownUpTraverser[A,This]]
      extends Impl[A,This] {
    this: This =>

    final protected def downUpForeach[U](down: A => Unit, up  : NodeT => Unit): Unit =
      Runner(noNode, down).dfsStack(up)

    final def fUnit[U](f: A => U): A => Unit = (a: A) => f(a)

    final protected def edgeVisitor[U](f: (A) => U): (EdgeT) => U = empty
  }

  protected case class InnerNodeDownUpTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters     = Parameters(),
      override val subgraphNodes: NodeFilter     = anyNode,
      override val subgraphEdges: EdgeFilter     = anyEdge,
      override val ordering     : ElemOrdering   = NoOrdering,
      override val maxWeight    : Option[Weight] = None)
      extends super.InnerNodeDownUpTraverser
         with DownUpTraverser[(Boolean, NodeT),InnerNodeDownUpTraverser] {
    
    final protected def newTraverser:
      (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => InnerNodeDownUpTraverser = copy
        
    final override def foreach[U](f: ((Boolean, NodeT)) => U): Unit = downUpForeach(
      fUnit(f),
      (n: NodeT) => f(false, n)
    )
    
    final protected def nodeVisitor[U](f: ((Boolean, NodeT)) => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(true, n) else empty 
  }
  
  def innerNodeDownUpTraverser(
      root         : NodeT,
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    InnerNodeDownUpTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class OuterNodeDownUpTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters     = Parameters(),
      override val subgraphNodes: NodeFilter     = anyNode,
      override val subgraphEdges: EdgeFilter     = anyEdge,
      override val ordering     : ElemOrdering   = NoOrdering,
      override val maxWeight    : Option[Weight] = None)
      extends super.OuterNodeDownUpTraverser
         with DownUpTraverser[(Boolean, N),OuterNodeDownUpTraverser] {
    
    final protected def newTraverser:
      (NodeT, Parameters, NodeFilter, EdgeFilter, ElemOrdering, Option[Weight]) => OuterNodeDownUpTraverser = copy

    final override def foreach[U](f: ((Boolean, N)) => U): Unit = downUpForeach(
      fUnit(f),
      (n: NodeT) => f(false, n)
    )

    final protected def nodeVisitor[U](f: ((Boolean, N)) => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(true,  n.value) else empty 
  }
  
  def outerNodeDownUpTraverser(
      root         : NodeT,
      parameters   : Parameters     = Parameters(),
      subgraphNodes: NodeFilter     = anyNode,
      subgraphEdges: EdgeFilter     = anyEdge,
      ordering     : ElemOrdering   = NoOrdering,
      maxWeight    : Option[Weight] = None) =
    OuterNodeDownUpTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  /** Efficient reverse `foreach` overcoming `ArrayStack`'s deficiency
   *  not to overwrite `reverseIterator`.
   */
  final protected class ReverseStackTraversable[S,T](
        s: Seq[S],
        toT: S => T,
        dropWhile: Option[S => Boolean] = None,
        enclosed: Array[Option[S]] = Array[Option[S]](None, None)
      ) extends Traversable[T] {
    
    @inline def foreach[U](f: T => U): Unit = source foreach (s => f(toT(s)))
    
    override def stringPrefix = "Nodes"
      
    private[this] var _size: Option[Int] = None
    @inline override val size: Int = _size getOrElse super.size

    @inline override def last: T = enclosed(1).fold(ifEmpty = toT(s.head))(toT)
    def reverse: Traversable[T] = new AbstractTraversable[T] {
      def foreach[U](f: T => U): Unit = {
        def fT(elem: S): Unit = f(toT(elem))
        def end(i: Int) = enclosed(i) foreach fT
        end(1)
        s foreach fT
        end(0)
      }
    }
    
    private lazy val upper = dropWhile.fold(ifEmpty = s.size){ pred =>
      var i = s.size - 1
      while (i >= 0 && pred(s(i)))
        i -= 1
      if (i < 0) 0 else i
    } 
    
    lazy val source: Traversable[S] = new AbstractTraversable[S] {
      def foreach[U](f: S => U): Unit = {
        enclosed(0) foreach f
        var i = upper
        var size = i
        while (i > 0) {
          i -= 1
          f(s(i))
        }
        enclosed(1) foreach f
        if (_size.isEmpty) _size = Some(size + enclosed.count(_.isDefined))
      }
    }
  }
  
  /** Enables lazy traversing of a `Map` with `key = source, value = target`.
   */
  final protected class MapPathTraversable[T](map: MMap[T,T], to: T, start: T)
      extends Traversable[T] {
    
    override def stringPrefix = "Nodes"

    private lazy val s: Seq[T] = {
      val stack = Stack.empty[T]
      @tailrec def loop(k: T): Unit = {
        val opt = map.get(k)
        if (opt.isDefined) {
          stack push k
          loop(opt.get)
        }
      }
      loop(to)
      stack push start
      stack
    }
      
    @inline def foreach[U](f: T => U): Unit = s foreach f
  }

  /** Path based on the passed collection of nodes with lazy evaluation of edges.
   */
  protected abstract class LazyPath(val nodes : Traversable[NodeT])
      extends Path {

    def startNode = nodes.head
    def endNode   = nodes.last

    private type AnyGraph = GraphTraversalImpl[N,E]
    
    override def equals(other: Any) = other match {
      case that: AnyGraph#Path => 
        (this eq that) ||
        that.toArray[AnyGraph#InnerElem].sameElements(toArray[InnerElem])
      case _ => false
    }
    override def hashCode = nodes.## + 27 * edges.##  
  }
  
  /** `LazyPath` with deferred edges selection.
   */
  protected abstract class SimpleLazyPath(override val nodes : Traversable[NodeT])
      extends LazyPath(nodes) {
    
    final lazy val edges = {
      val buf = new ArrayBuffer[EdgeT](nodes.size) {
        override def stringPrefix = "Edges"
      }
      (nodes.head /: nodes.tail){ (prev: NodeT, n: NodeT) =>
        buf += selectEdge(prev, n)
        n
      }
      buf
    }

    protected def selectEdge(from: NodeT, to: NodeT): EdgeT
  }

  /** `LazyPath` where edges are selected by taking the first one fitting.
   */
  protected class AnyEdgeLazyPath(override val nodes : Traversable[NodeT], edgeFilter: EdgeFilter)
      extends SimpleLazyPath(nodes) {
    
    final protected def selectEdge(from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        (from outgoingTo to find edgeFilter).get
      else
        (from findOutgoingTo to).get
  }

  /** `LazyPath` with edges selected by minimal weight.
   */
  protected class MinWeightEdgeLazyPath(override val nodes : Traversable[NodeT],
                                        edgeFilter: EdgeFilter,
                                        weightOrdering: Ordering[EdgeT])
      extends SimpleLazyPath(nodes) {
    
    final def selectEdge (from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        from outgoingTo to withFilter edgeFilter min weightOrdering
      else
        from outgoingTo to min weightOrdering
  }

  /** `LazyPath` with edge selection such that there exists no duplicate edge in the path.
   */
  protected class MultiEdgeLazyPath(
      override val nodes : ReverseStackTraversable[CycleStackElem, NodeT],
      edgeFilter: EdgeFilter)
      extends LazyPath(nodes) {
    
    final protected val multi = new EqHashSet[EdgeT](graphSize / 2) 
        
    final lazy val edges = {
      val buf = new ArrayBuffer[EdgeT](nodes.size) {
        override def stringPrefix = "Edges"
      }
      val isDiGraph = thisGraph.isDirected
      (nodes.head /: nodes.source.tail){ (prev: NodeT, elem: CycleStackElem) =>
        val (n, conn) = elem
        def get(edges: Iterable[EdgeT], pred: EdgeFilter) = {
          def ok(e: EdgeT): Boolean = ! multi.contains(e) && edgeFilter(e) && pred(e) 
          if (isDiGraph)
            (edges find ok).get
          else {
            val (di, unDi) = edges filter ok partition (_.isDirected)
            di.headOption getOrElse unDi.head
          }
        }
        val edge = (conn.size: @switch) match {
          case 0 => get(n.edges, (e: EdgeT) => e.hasTarget((x: NodeT) => x eq prev))          
          case 1 => conn.head
          case _ => get(conn,    (e: EdgeT) => e.hasSource((x: NodeT) => x eq n)) 
        }
        buf += edge
        multi += edge
        n
      }
      multi.clear
      buf
    }
  }
  
  protected class AnyEdgeLazyCycle(override val nodes : Traversable[NodeT], edgeFilter: EdgeFilter)
      extends AnyEdgeLazyPath(nodes, edgeFilter)
         with Cycle    

  protected class MultiEdgeLazyCycle(
      override val nodes : ReverseStackTraversable[CycleStackElem, NodeT], edgeFilter: EdgeFilter)
      extends MultiEdgeLazyPath(nodes, edgeFilter)
         with Cycle    
}
object GraphTraversalImpl {
  import GraphTraversal._
  type Depth = Int

  /** Extended node visitor informer for depth first searches. 
   */
  trait DfsInformer[N] extends NodeInformer {
    import DfsInformer._
    def stackIterator: DfsStack[N]
    def pathIterator:  DfsPath [N]
  }
  object DfsInformer {
    case class Element[N] protected[collection](node: N, depth: Depth, cumulatedWeight: Double = 0)
    type DfsStack[N] = Iterator[Element[N]]
    type DfsPath [N] = Iterator[Element[N]]
    def unapply[N](inf: DfsInformer[N]): Option[(DfsStack[N], DfsPath[N])] =
      Some(inf.stackIterator, inf.pathIterator)
  }
  /** Extended node visitor informer for cycle detecting. 
   *  This informer always returns `0` for `depth`.
   */
  trait WgbInformer[N,E] extends NodeInformer {
    import WgbInformer._
    def stackIterator: WgbStack[N,E]
    def pathIterator:  WgbPath [N,E]
  }
  object WgbInformer {
    case class Element[N,+E] protected[collection]( // exclude and multiEdges only needed for undirected edges
        node: N, predecessor: N, exclude: Boolean, multiEdges: Iterable[E], cumWeight: Double = 0)
    type WgbStack[N,E] = Iterator[Element[N,E]]
    type WgbPath [N,E] = Iterator[(N, Iterable[E])]
    def unapply[N,E](inf: WgbInformer[N,E]): Option[(WgbStack[N,E], WgbPath[N,E])] =
      Some(inf.stackIterator, inf.pathIterator)
  }
  /** Extended node visitor informer for best first searches. 
   */
  trait BfsInformer[N] extends NodeInformer {
    import BfsInformer._
    def queueIterator: BfsQueue[N]
  }
  object BfsInformer {
    type Element[N] = DfsInformer.Element[N]
    val  Element    = DfsInformer.Element
    type BfsQueue[N] = Iterator[Element[N]]
    def unapply[N](inf: BfsInformer[N]): Option[BfsQueue[N]] =
      Some(inf.queueIterator)
  }
  /** Extended node visitor informer for calculating shortest paths. 
   *  This informer always returns `0` for `depth`.
   */
  abstract class DijkstraInformer[N, T: Numeric] extends NodeInformer {
    import DijkstraInformer._
    def queueIterator: DijkstraQueue[N,T]
    def costsIterator: DijkstraCosts[N,T]
  }
  object DijkstraInformer {
    case class Element[N,T: Numeric] protected[collection](node: N, cumWeight: T, depth: Depth)
    type DijkstraQueue[N,T] = Iterator[Element[N,T]]
    type DijkstraCosts[N,T] = Iterator[(N,T)]
    def unapply[N, T: Numeric](inf: DijkstraInformer[N,T])
        : Option[(DijkstraQueue[N,T], DijkstraCosts[N,T])] =
      Some(inf.queueIterator, inf.costsIterator)
  }
} 
