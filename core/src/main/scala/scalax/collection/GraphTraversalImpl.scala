package scalax.collection

import scala.annotation.{switch, tailrec}
import scala.collection.{AbstractIterable, EqSetFacade, IndexedSeq, Seq}
import scala.collection.mutable.{ArrayBuffer, Buffer, Map => MMap, Stack}

import GraphEdge.EdgeLike
import mutable.{EqHashMap, EqHashSet}

/** Default implementation of the functionality defined by [[GraphTraversal]]
  *  except for algorithms that are placed in [[TraverserImpl]].
  *
  *  @author Peter Empen
  */
trait GraphTraversalImpl[N, E <: EdgeLike[N]] extends GraphTraversal[N, E] with TraverserImpl[N, E] with State[N, E] {
  thisGraph: TraverserImpl[N, E] =>

  import GraphTraversal._
  import Informer.{CycleStackElem, NodeElement}
  import Informer.DfsInformer.{Element => DfsElem}
  import Visitor._
  import State._

  final protected def cycle(
      maybeStart: Option[NodeT],
      stack: Stack[DfsElem],
      edgeFilter: EdgePredicate
  ): Option[Cycle] =
    maybeStart map { start =>
      new AnyEdgeLazyCycle(
        new ReverseStackTraversable[DfsElem](stack, None, Array[Option[DfsElem]](None, Some(DfsElem(start)))),
        edgeFilter
      )
    }

  final protected def cycle(results: Option[(NodeT, Stack[CycleStackElem])], edgeFilter: EdgePredicate): Option[Cycle] =
    results match {
      case Some((start, stack)) =>
        val reverse = new ReverseStackTraversable[CycleStackElem](
          stack,
          Some((elem: CycleStackElem) => elem.node ne start),
          Array.fill[Option[CycleStackElem]](2)(Some(CycleStackElem(start)))
        )
        Some(
          if (thisGraph.isDirected) new AnyEdgeLazyCycle(reverse, edgeFilter)
          else new MultiEdgeLazyCycle(reverse, edgeFilter)
        )
      case _ => None
    }

  class WalkBuilderImpl(
      override val start: NodeT,
      sizeHint: Int = defaultPathSize,
      edgeSelector: (NodeT, NodeT) => Option[EdgeT]
  ) extends WalkBuilder {
    self =>

    protected[this] var lastNode: Option[NodeT] = Some(start)
    private[this] var lastEdge: Option[EdgeT]   = None
    protected[this] val nodes                   = new ArrayBuffer[NodeT](sizeHint) += start
    protected[this] val edges                   = new ArrayBuffer[EdgeT](sizeHint)

    def add(node: NodeT): Boolean =
      if (
        lastNode.fold[Boolean](
          // lastEdge, node
          ifEmpty = lastEdge.get.hasTarget(node)
        )(
          // lastNode, node
          edgeSelector(_, node).fold(ifEmpty = false) { e =>
            edges += e
            true
          }
        )
      ) {
        nodes += node
        lastNode = Some(node)
        lastEdge = None
        true
      } else false

    def add(edge: EdgeT): Boolean =
      if (
        lastEdge.fold[Boolean](
          // lastNode, edge
          ifEmpty = edge.hasSource(lastNode.get)
        ) {
          // lastEdge, edge
          lastEdge =>
            var sources, targets = Set.empty[NodeT]
            edge.withSources(sources += _)
            lastEdge.withTargets(targets += _)
            val intersection = sources intersect targets
            if (intersection.isEmpty) false
            else
              select(intersection).fold[Boolean](false) { n =>
                nodes += n
                true
              }
        }
      ) {
        edges += edge
        lastNode = None
        lastEdge = Some(edge)
        true
      } else false

    /* @param fromNodes Non-empty set of nodes to select from.
     */
    protected def select(fromNodes: Set[NodeT]): Option[NodeT] =
      Some(fromNodes.head)

    def clear(): Unit = {
      nodes.clear(); nodes += start
      edges.clear()
      lastNode = Some(start)
      lastEdge = None
    }

    final protected def resultEdges = lastEdge.fold[IndexedSeq[EdgeT]](
      ifEmpty = edges
    )(_ => edges.slice(0, edges.size - 1))

    def result(): Walk = new Walk {
      val nodes     = self.nodes
      def edges     = resultEdges
      val startNode = start
      val endNode   = nodes(nodes.size - 1)
    }
  }

  def newWalkBuilder(
      start: NodeT
  )(implicit sizeHint: Int = defaultPathSize, edgeSelector: (NodeT, NodeT) => Option[EdgeT]): WalkBuilderImpl =
    new WalkBuilderImpl(start, sizeHint, edgeSelector)

  class PathBuilderImpl(
      override val start: NodeT,
      sizeHint: Int = defaultPathSize,
      edgeSelector: (NodeT, NodeT) => Option[EdgeT]
  ) extends WalkBuilderImpl(start, sizeHint, edgeSelector)
      with PathBuilder {
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
      fromNodes find (!uniqueNodes(_))

    override def clear(): Unit = {
      super.clear()
      uniqueNodes.clear()
      uniqueNodes += start
    }

    override def result(): Path = new Path {
      val nodes     = self.nodes
      val edges     = resultEdges
      val startNode = start
      val endNode   = nodes(nodes.size - 1)
    }
  }

  def newPathBuilder(
      start: NodeT
  )(implicit sizeHint: Int = defaultPathSize, edgeSelector: (NodeT, NodeT) => Option[EdgeT]): PathBuilderImpl =
    new PathBuilderImpl(start, sizeHint, edgeSelector)

  type NodeT <: InnerNodeTraversalImpl
  trait InnerNodeTraversalImpl extends TraverserInnerNode with InnerNodeState {
    this: NodeT =>
  }

  protected class WeakComponentImpl(
      override val root: NodeT,
      override val parameters: Parameters,
      override val subgraphNodes: NodePredicate,
      override val subgraphEdges: EdgePredicate,
      override val ordering: ElemOrdering,
      override val nodes: Set[NodeT]
  ) extends Component {

    final protected def mayHaveFrontierEdges: Boolean = false
    final protected def className                     = "WeakComponent"
  }

  protected class StrongComponentImpl(
      override val root: NodeT,
      override val parameters: Parameters,
      override val subgraphNodes: NodePredicate,
      override val subgraphEdges: EdgePredicate,
      override val ordering: ElemOrdering,
      override val nodes: Set[NodeT]
  ) extends Component {

    final protected def mayHaveFrontierEdges: Boolean = true
    final protected def className                     = "StrongComponent"
  }

  final protected def expectedMaxNodes(divisor: Int, min: Int = 128): Int = {
    val o = order
    if (o == 0) 1
    else math.min(o, math.max(o / divisor, min))
  }

  protected type TopoSortSetup = (Buffer[NodeT], MMap[NodeT, Int], Option[NodeT])

  /** Calculates in-degrees of nodes spanned by `traversable`.
    *
    *  @param nodes supplies the nodes for which the degree is to be calculated
    *  @param maybeHandle to be used to mark visited nodes
    *  @param includeAnyway include this node in the resulting list of nodes without predecessors
    *         irrespective of its in degree
    *  @param includeInDegree optionally filters predecessor nodes when calculating the in degree
    *  @return triple of
    *          a. nodes without predecessors in the component spanned by `traverser`
    *          a. map of visited nodes to their in degrees
    *          a. size of `traversable`
    */
  final protected def forInDegrees(
      nodes: Iterable[NodeT] with SubgraphProperties,
      maybeHandle: Option[Handle] = None,
      includeAnyway: Option[NodeT] = None,
      includeInDegree: NodePredicate = anyNode,
      fillInDegrees: Boolean = true
  ): TopoSortSetup = {

    val nodesWithoutPredecessor       = new ArrayBuffer[NodeT](expectedMaxNodes(1000))
    val nodeInDegrees                 = new EqHashMap[NodeT, Int](if (fillInDegrees) order else 0)
    var inspectedNode: Option[NodeT]  = None
    def nodeFilter(n: NodeT): Boolean = nodes.subgraphNodes(n) && includeInDegree(n)
    nodes foreach { n =>
      maybeHandle foreach (implicit h => n.visited = true)
      val inDegree = n.inDegree(nodeFilter, nodes.subgraphEdges)
      if (fillInDegrees) nodeInDegrees put (n, inDegree)
      if (inDegree == 0 || (n eq includeAnyway.orNull)) nodesWithoutPredecessor += n
      else inspectedNode = inspectedNode orElse Some(n)
    }
    (nodesWithoutPredecessor, nodeInDegrees, inspectedNode)
  }

  protected case class ComponentTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters,
      override val subgraphNodes: NodePredicate,
      override val subgraphEdges: EdgePredicate,
      override val ordering: ElemOrdering,
      override val maxWeight: Option[Weight]
  ) extends ComponentTraverser {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => ComponentTraverserImpl =
      copy

    final private def innerElemTraverser =
      InnerElemTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering)

    protected lazy val components: Iterable[WeakComponentImpl] = {
      val traverser =
        InnerNodeTraverserImpl(root, parameters withDirection AnyConnected, subgraphNodes, subgraphEdges, ordering)
      withHandle() { implicit visitedHandle =>
        for (node <- nodes if !node.visited && subgraphNodes(node)) yield {
          val componentNodes = new ArrayBuffer[NodeT](expectedMaxNodes(3))
          traverser.withRoot(node) foreach { n =>
            n.visited = true
            componentNodes += n
          }
          new WeakComponentImpl(
            node,
            parameters,
            subgraphNodes,
            subgraphEdges,
            ordering,
            new EqSetFacade(componentNodes)
          )
        }
      }
    }

    override def iterator = components.iterator

    def findCycle[U](implicit visitor: InnerElem => U = Visitor.empty): Option[Cycle] =
      if (order == 0) None
      else {
        val traverser = innerElemTraverser
        withHandles(2) { handles =>
          implicit val visitedHandle: State.Handle = handles(0)
          for (node <- nodes if !node.visited && subgraphNodes(node)) {
            val nodeTraverser: InnerElemTraverserImpl =
              traverser.withRoot(node) // TODO not sure why this declaration is needed
            val res = nodeTraverser.Runner(noNode, visitor).dfsWGB(handles)
            if (res.isDefined)
              return cycle(res, subgraphEdges)
          }
        }
        None
      }

    final def topologicalSort[U](implicit visitor: InnerElem => U = Visitor.empty): CycleNodeOrTopologicalOrder =
      innerElemTraverser
        .Runner(noNode, visitor)
        .topologicalSort(forInDegrees(SubgraphProperties(nodes, subgraphNodes, subgraphEdges)))

    final def topologicalSortByComponent[U](implicit
        visitor: InnerElem => U = Visitor.empty
    ): Iterable[CycleNodeOrTopologicalOrder] =
      if (order == 0) Nil
      else {
        val topoRunner    = innerElemTraverser.Runner(noNode, visitor)
        val forStartNodes = innerNodeTraverser(root, Parameters.Dfs(AnyConnected))
        withHandles(2) { handles: Array[Handle] =>
          val (startNodesHandle, topoHandle) = (Some(handles(0)), Some(handles(1)))
          implicit val handle: State.Handle  = startNodesHandle.get
          for (node <- nodes if !node.visited && subgraphNodes(node))
            yield topoRunner.topologicalSort(
              forInDegrees(forStartNodes.withRoot(node), startNodesHandle, includeInDegree = subgraphNodes),
              topoHandle
            )
        }
      }
  }

  def componentTraverser(
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    ComponentTraverserImpl(null.asInstanceOf[NodeT], parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class StrongComponentTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters,
      override val subgraphNodes: NodePredicate,
      override val subgraphEdges: EdgePredicate,
      override val ordering: ElemOrdering,
      override val maxWeight: Option[Weight]
  ) extends StrongComponentTraverser {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => StrongComponentTraverserImpl =
      copy

    protected lazy val components: Iterable[Component] = {
      val traverser =
        InnerNodeTraverserImpl(root, parameters withDirection Successors, subgraphNodes, subgraphEdges, ordering)
      withHandle() { implicit handle =>
        (for (node <- nodes if !node.visited && subgraphNodes(node))
          yield {
            val nodeTraverser: InnerNodeTraverserImpl = traverser.withRoot(node)
            nodeTraverser.Runner(noNode, Visitor.empty).dfsTarjan(Some(handle))
          }).flatten
      }
    }

    override def iterator = components.iterator
  }

  def strongComponentTraverser(
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    StrongComponentTraverserImpl(
      null.asInstanceOf[NodeT],
      parameters,
      subgraphNodes,
      subgraphEdges,
      ordering,
      maxWeight
    )

  protected case class InnerNodeTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters = Parameters(),
      override val subgraphNodes: NodePredicate = anyNode,
      override val subgraphEdges: EdgePredicate = anyEdge,
      override val ordering: ElemOrdering = NoOrdering,
      override val maxWeight: Option[Weight] = None
  ) extends InnerNodeTraverser
      with Impl[NodeT, InnerNodeTraverserImpl] {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => InnerNodeTraverserImpl =
      copy

    final protected def nodeVisitor[U](f: NodeT => U): (NodeT) => U = f
    final protected def edgeVisitor[U](f: NodeT => U): (EdgeT) => U = Visitor.empty
  }

  def innerNodeTraverser(
      root: NodeT,
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    InnerNodeTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class OuterNodeTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters = Parameters(),
      override val subgraphNodes: NodePredicate = anyNode,
      override val subgraphEdges: EdgePredicate = anyEdge,
      override val ordering: ElemOrdering = NoOrdering,
      override val maxWeight: Option[Weight] = None
  ) extends OuterNodeTraverser
      with Impl[N, OuterNodeTraverserImpl] {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => OuterNodeTraverserImpl =
      copy

    final protected def nodeVisitor[U](f: N => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(n.outer) else Visitor.empty

    final protected def edgeVisitor[U](f: N => U): (EdgeT) => U = Visitor.empty
  }

  def outerNodeTraverser(
      root: NodeT,
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    OuterNodeTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class InnerEdgeTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters = Parameters(),
      override val subgraphNodes: NodePredicate = anyNode,
      override val subgraphEdges: EdgePredicate = anyEdge,
      override val ordering: ElemOrdering = NoOrdering,
      override val maxWeight: Option[Weight] = None
  ) extends InnerEdgeTraverser
      with Impl[EdgeT, InnerEdgeTraverserImpl] {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => InnerEdgeTraverserImpl =
      copy

    final protected def nodeVisitor[U](f: EdgeT => U): (NodeT) => U = Visitor.empty
    final protected def edgeVisitor[U](f: EdgeT => U): (EdgeT) => U =
      if (isDefined(f)) (e: EdgeT) => f(e) else Visitor.empty
  }

  def innerEdgeTraverser(
      root: NodeT,
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    InnerEdgeTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class OuterEdgeTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters = Parameters(),
      override val subgraphNodes: NodePredicate = anyNode,
      override val subgraphEdges: EdgePredicate = anyEdge,
      override val ordering: ElemOrdering = NoOrdering,
      override val maxWeight: Option[Weight] = None
  ) extends OuterEdgeTraverser
      with Impl[E, OuterEdgeTraverserImpl] {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => OuterEdgeTraverserImpl =
      copy

    final protected def nodeVisitor[U](f: E => U): (NodeT) => U = Visitor.empty
    final protected def edgeVisitor[U](f: E => U): (EdgeT) => U =
      if (isDefined(f)) (e: EdgeT) => f(e.outer) else Visitor.empty
  }

  def outerEdgeTraverser(
      root: NodeT,
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    OuterEdgeTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class InnerElemTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters = Parameters(),
      override val subgraphNodes: NodePredicate = anyNode,
      override val subgraphEdges: EdgePredicate = anyEdge,
      override val ordering: ElemOrdering = NoOrdering,
      override val maxWeight: Option[Weight] = None
  ) extends InnerElemTraverser
      with Impl[InnerElem, InnerElemTraverserImpl] {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => InnerElemTraverserImpl =
      copy

    final protected def nodeVisitor[U](f: InnerElem => U): (NodeT) => U = if (isDefined(f)) (n: NodeT) => f(n) else f
    final protected def edgeVisitor[U](f: InnerElem => U): (EdgeT) => U = if (isDefined(f)) (e: EdgeT) => f(e) else f
  }

  def innerElemTraverser(
      root: NodeT,
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    InnerElemTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class OuterElemTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters = Parameters(),
      override val subgraphNodes: NodePredicate = anyNode,
      override val subgraphEdges: EdgePredicate = anyEdge,
      override val ordering: ElemOrdering = NoOrdering,
      override val maxWeight: Option[Weight] = None
  ) extends OuterElemTraverser
      with Impl[OuterElem, OuterElemTraverserImpl] {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => OuterElemTraverserImpl =
      copy

    final protected def nodeVisitor[U](f: OuterElem => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(OuterNode(n.outer))
      else Visitor.empty

    final protected def edgeVisitor[U](f: OuterElem => U): (EdgeT) => U =
      if (isDefined(f)) (e: EdgeT) => f(OuterEdge(e.outer))
      else Visitor.empty
  }

  def outerElemTraverser(
      root: NodeT,
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    OuterElemTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected trait DownUpTraverser[A, +This <: DownUpTraverser[A, This]] extends Impl[A, This] {
    this: This =>

    final protected def downUpForeach[U](down: A => Unit, up: NodeT => Unit): Unit =
      Runner(noNode, down).dfsStack(up)

    final def fUnit[U](f: A => U): A => Unit = (a: A) => f(a)

    final protected def edgeVisitor[U](f: (A) => U): (EdgeT) => U = Visitor.empty
  }

  protected case class InnerNodeDownUpTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters = Parameters(),
      override val subgraphNodes: NodePredicate = anyNode,
      override val subgraphEdges: EdgePredicate = anyEdge,
      override val ordering: ElemOrdering = NoOrdering,
      override val maxWeight: Option[Weight] = None
  ) extends InnerNodeDownUpTraverser
      with DownUpTraverser[(Boolean, NodeT), InnerNodeDownUpTraverserImpl] {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => InnerNodeDownUpTraverserImpl =
      copy

    final override protected def autarkicForeach[U](f: ((Boolean, NodeT)) => U): Unit = downUpForeach(
      fUnit(f),
      (n: NodeT) => f(false, n)
    )

    final protected def nodeVisitor[U](f: ((Boolean, NodeT)) => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(true, n) else Visitor.empty
  }

  def innerNodeDownUpTraverser(
      root: NodeT,
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    InnerNodeDownUpTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  protected case class OuterNodeDownUpTraverserImpl(
      override val root: NodeT,
      override val parameters: Parameters = Parameters(),
      override val subgraphNodes: NodePredicate = anyNode,
      override val subgraphEdges: EdgePredicate = anyEdge,
      override val ordering: ElemOrdering = NoOrdering,
      override val maxWeight: Option[Weight] = None
  ) extends OuterNodeDownUpTraverser
      with DownUpTraverser[(Boolean, N), OuterNodeDownUpTraverserImpl] {

    final protected def newTraverser
        : (NodeT, Parameters, NodePredicate, EdgePredicate, ElemOrdering, Option[Weight]) => OuterNodeDownUpTraverserImpl =
      copy

    final override protected def autarkicForeach[U](f: ((Boolean, N)) => U): Unit = downUpForeach(
      fUnit(f),
      (n: NodeT) => f(false, n)
    )

    final protected def nodeVisitor[U](f: ((Boolean, N)) => U): (NodeT) => U =
      if (isDefined(f)) (n: NodeT) => f(true, n.outer) else Visitor.empty
  }

  def outerNodeDownUpTraverser(
      root: NodeT,
      parameters: Parameters = Parameters(),
      subgraphNodes: NodePredicate = anyNode,
      subgraphEdges: EdgePredicate = anyEdge,
      ordering: ElemOrdering = NoOrdering,
      maxWeight: Option[Weight] = None
  ) =
    OuterNodeDownUpTraverserImpl(root, parameters, subgraphNodes, subgraphEdges, ordering, maxWeight)

  /** Efficient reverse `foreach` overcoming `Stack`'s deficiency not to overwrite `reverseIterator`.
    */
  // TODO is this still needed? Stack now _does_ override `reverseIterator`.
  final protected class ReverseStackTraversable[S <: NodeElement](
      s: IndexedSeq[S],
      takeWhile: Option[S => Boolean] = None,
      enclosed: Array[Option[S]] = Array[Option[S]](None, None)
  ) extends Iterable[NodeT] {

    override def iterator = source.map(_.node).iterator

    final override protected def className = "Nodes"

    private[this] var _size: Option[Int] = None
    @inline override val size: Int       = _size getOrElse super.size

    @inline override def last: NodeT = enclosed(1).fold(ifEmpty = s.head.node)(_.node)

    // TODO unreachable?
    def reverse: Iterable[NodeT] = new AbstractIterable[NodeT] {
      override def iterator = ???
      /* TODO replace foreach with iterator
      def foreach[U](f: NodeT => U): Unit = {
        def fT(elem: S): Unit = f(elem.node)
        def end(i: Int): Unit = enclosed(i) foreach fT
        end(1)
        s foreach fT
        end(0)
      }
       */
    }

    private lazy val upper: Int = takeWhile.fold(ifEmpty = s.size) { pred =>
      var i = s.size - 1
      while (i >= 0 && pred(s(i))) i -= 1
      if (i < 0) 0 else i
    }

    private[GraphTraversalImpl] lazy val source: Iterable[S] = new AbstractIterable[S] {
      override def iterator = {
        val buffer = ArrayBuffer[S]()
        foreach(buffer += _)
        buffer.iterator
      }
      override def foreach[U](f: S => U): Unit = {
        enclosed(0) foreach f
        var i = upper
        while (i > 0) {
          i -= 1
          f(s(i))
        }
        enclosed(1) foreach f
        if (_size.isEmpty) _size = Some(upper + enclosed.count(_.isDefined))
      }
    }
  }

  /** Enables lazy traversing of a `Map` with `key = source, value = target`.
    */
  final protected class MapPathTraversable[T](map: MMap[T, T], to: T, start: T) extends Iterable[T] {

    final override protected def className = "Nodes"

    private lazy val s: Seq[T] = {
      val stack: Stack[T] = Stack.empty[T]
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

    override def iterator = s.iterator
  }

  /** Path based on the passed collection of nodes with lazy evaluation of edges.
    */
  abstract protected class LazyPath(val nodes: Iterable[NodeT]) extends Path {

    def startNode: NodeT = nodes.head
    def endNode: NodeT   = nodes.last

    private type AnyGraph =
      GraphTraversalImpl[N, E] // scalafix warning not correct, see https://github.com/scalacenter/scalafix/issues/969

    override def equals(other: Any): Boolean = other match {
      case that: AnyGraph#Path =>
        (this eq that) ||
        that.toArray[AnyGraph#InnerElem].sameElements(toArray[InnerElem])
      case _ => false
    }
    override def hashCode: Int = nodes.## + 27 * edges.##
  }

  /** `LazyPath` with deferred edges selection.
    */
  abstract protected class SimpleLazyPath(override val nodes: Iterable[NodeT]) extends LazyPath(nodes) {

    final lazy val edges = {
      val buf = new ArrayBuffer[EdgeT](nodes.size) {
        final override protected def className = "Edges"
      }
      nodes.tail.foldLeft(nodes.head) { (prev: NodeT, n: NodeT) =>
        buf += selectEdge(prev, n)
        n
      }
      buf
    }

    protected def selectEdge(from: NodeT, to: NodeT): EdgeT
  }

  /** `LazyPath` where edges are selected by taking the first one fitting.
    */
  protected class AnyEdgeLazyPath(override val nodes: Iterable[NodeT], edgeFilter: EdgePredicate)
      extends SimpleLazyPath(nodes) {

    final protected def selectEdge(from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        (from outgoingTo to find edgeFilter).get
      else
        (from findOutgoingTo to).get
  }

  /** `LazyPath` with edges selected by minimal weight.
    */
  protected class MinWeightEdgeLazyPath(
      override val nodes: Iterable[NodeT],
      edgeFilter: EdgePredicate,
      weightOrdering: Ordering[EdgeT]
  ) extends SimpleLazyPath(nodes) {

    final def selectEdge(from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        (from outgoingTo to).withSetFilter(edgeFilter) min weightOrdering
      else
        from outgoingTo to min weightOrdering
  }

  /** `LazyPath` with edge selection such that there exists no duplicate edge in the path.
    */
  protected class MultiEdgeLazyPath(
      override val nodes: ReverseStackTraversable[CycleStackElem],
      edgeFilter: EdgePredicate
  ) extends LazyPath(nodes) {

    final protected val multi = new EqHashSet[EdgeT](thisGraph.size / 2)

    final lazy val edges = {
      val buf = new ArrayBuffer[EdgeT](nodes.size) {
        final override protected def className = "Edges"
      }
      val isDiGraph = thisGraph.isDirected
      nodes.source.tail.foldLeft(nodes.head) { (prev: NodeT, elem: CycleStackElem) =>
        val CycleStackElem(n, conn) = elem
        def get(edges: Iterable[EdgeT], pred: EdgePredicate): EdgeT = {
          def ok(e: EdgeT): Boolean = !multi.contains(e) && edgeFilter(e) && pred(e)
          if (isDiGraph)
            (edges find ok).get
          else {
            val (di, unDi) = edges filter ok partition (_.isDirected)
            di.headOption getOrElse unDi.head
          }
        }
        val edge = (conn.size: @switch) match {
          case 0 => get(n.edges, (e: EdgeT) => e.hasSource((x: NodeT) => x eq prev))
          case 1 => conn.head
          case _ => get(conn, (e: EdgeT) => e.hasTarget((x: NodeT) => x eq n))
        }
        buf += edge
        multi += edge
        n
      }
      multi.clear()
      buf
    }
  }

  protected class AnyEdgeLazyCycle(override val nodes: Iterable[NodeT], edgeFilter: EdgePredicate)
      extends AnyEdgeLazyPath(nodes, edgeFilter)
      with Cycle

  protected class MultiEdgeLazyCycle(
      override val nodes: ReverseStackTraversable[CycleStackElem],
      edgeFilter: EdgePredicate
  ) extends MultiEdgeLazyPath(nodes, edgeFilter)
      with Cycle
}
