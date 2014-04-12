package scalax.collection

import language.{higherKinds, implicitConversions}
import scala.annotation.{switch, tailrec}
import collection.mutable.{ArrayBuffer, ArraySeq, ArrayStack => Stack, ListBuffer, Queue,
                           PriorityQueue, Set => MutableSet, Map => MutableMap}

import collection.{Abstract, EqSetFacade}
import GraphPredef.{EdgeLikeIn, Param, InParam, OutParam,
                    OuterNode, InnerNodeParam, OuterEdge, OuterElem, InnerEdgeParam}
import GraphEdge.{EdgeLike}
import mutable.{ArraySet, ExtBitSet}
import scalax.collection.mutable.ExtBitSet
import scalax.collection.mutable.ExtBitSet

trait GraphTraversalImpl[N, E[X] <: EdgeLikeIn[X]]
  extends GraphTraversal[N,E]
  with State[N,E]
{ thisGraph =>

  import GraphTraversalImpl._
  import GraphTraversal.VisitorReturn._
  import GraphTraversal._
  import State._
  
  protected type CycleStackElem = (NodeT, Iterable[EdgeT])
  final protected def cycle(results: (Option[NodeT], Stack[CycleStackElem]),
                            edgeFilter : (EdgeT) => Boolean): Option[Cycle] = {
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
    
  type NodeT <: InnerNodeTraversalImpl
  trait InnerNodeTraversalImpl extends TraverserInnerNode with InnerNodeState {
    this: NodeT =>
  }

  private[this] def emptyRoot: NodeT = null.asInstanceOf[NodeT]
  
  protected class ComponentImpl(
      override val root         : NodeT, 
      override val parameters   : Parameters,
      override val subgraphNodes: (NodeT) => Boolean,
      override val subgraphEdges: (EdgeT) => Boolean,
      override val ordering     : ElemOrdering,
      override val nodes        : Set[NodeT])
      extends Component {
    
    lazy val edges: Set[EdgeT] = {
      val edges = new ArrayBuffer[EdgeT](nodes.size * 2)
      for (n <- nodes) n.edges foreach (edges += _)
      new EqSetFacade(edges)
    }
  }
  
  protected case class ComponentTraverser(
      override val root         : NodeT              = emptyRoot, 
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.ComponentTraverser {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => ComponentTraverser = copy _

    protected lazy val components: Iterable[ComponentImpl] = {
      val components = new ArrayBuffer[NodeT]
      val traverser = InnerNodeTraverser(
          root, parameters, subgraphNodes, subgraphEdges, ordering)
      withHandle() {  implicit visitedHandle =>
        for (node <- nodes if ! node.visited && subgraphNodes(node)) yield {
          val componentNodes = new ArrayBuffer[NodeT](
              math.min(order, math.max(order / 6, 128)))
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
    
    def findCycle(implicit visitor: InnerElem => Unit = emptyVisitor): Option[Cycle] =
      if (order == 0) None
      else {
        val traverser = InnerElemTraverser(
            root, parameters, subgraphNodes, subgraphEdges, ordering)
        withHandles(2) { handles => 
          implicit val visitedHandle = handles(0)
          for (node <- nodes if ! node.visited && subgraphNodes(node)) {
            val res = traverser.withRoot(node). // compiler issue
                      asInstanceOf[InnerElemTraverser].depthFirstSearchWGB(handles) 
            if (res._1.isDefined)
              return cycle(res, subgraphEdges)
          }
        }
        None
      }
  }
  
  def componentTraverser(
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    ComponentTraverser(emptyRoot, parameters, subgraphNodes, subgraphEdges, ordering)
  
  protected trait Impl[A] {
    this: Traverser[A,_] =>

    final def pathUntil(pred: (NodeT) => Boolean)
                       (implicit visitor: A => Unit = emptyVisitor): Option[Path] = {
      val (target, path) = 
          new Traversal(Successors, subgraphNodes, subgraphEdges, nodeVisitor(visitor),
                        edgeVisitor(visitor), ordering)._depthFirstSearch(root, pred)
      target map { _ =>
        new AnyEdgeLazyPath(
          new ReverseStackTraversable[(NodeT, Int), NodeT]
              (path, (elem: (NodeT, Int)) => elem._1),
          subgraphEdges)
      }
    }

    final def shortestPathTo[T: Numeric](
        potentialSuccessor: NodeT,
        weight            : EdgeT => T,
        visitor           : A => Unit = emptyVisitor): Option[Path] = {
      withHandle() { implicit visitedHandle => 
        val num = implicitly[Numeric[T]] 
        import num._
        @inline def visited(n: NodeT) = n.visited

        val nodeVisitor = this.nodeVisitor(visitor) 
        val edgeVisitor = this.edgeVisitor(visitor)

        type NodeWeight = (NodeT,T)
        val weightOrdering = Edge.weightOrdering(weight)
        val dest      = MutableMap[NodeT,T](root -> zero)
        val mapToPred = MutableMap[NodeT,NodeT]()
        val traversal = new Traversal(Successors, subgraphNodes, subgraphEdges,
                                      nodeVisitor, edgeVisitor, ordering) 
        val doNodeVisitor = isCustomNodeVisitor(nodeVisitor)
        val extendedVisitor =
          if (doNodeVisitor)
            nodeVisitor match {
              case e: ExtendedNodeVisitor => Some(e)
              case _ => None
            }
          else None
        // not implicit due to issues #4405 and #4407
        object ordNodeWeight extends Ordering[NodeWeight] {
          def compare(x: NodeWeight,
                      y: NodeWeight) = num.compare(y._2, x._2)
        }
        val qNodes = new PriorityQueue[NodeWeight]()(ordNodeWeight) += ((root -> zero))
  
        def sortedAdjacentsNodes(node: NodeT): Option[PriorityQueue[NodeWeight]] = 
          traversal.filteredDiSuccessors(node, visited, false) match {
            case adj if adj.nonEmpty =>
              Some(adj.
                   foldLeft(new PriorityQueue[NodeWeight]()(ordNodeWeight))(
                     (q,n) => q += ((n, dest(node) +
                                        weight(node.outgoingTo(n).filter(subgraphEdges(_)).
                                               min(weightOrdering))))))
            case _ => None
          }
        def relax(pred: NodeT, succ: NodeT) {
          val cost = dest(pred) + weight(pred.outgoingTo(succ).filter(subgraphEdges(_)).
                                         min(weightOrdering))
          if(!dest.isDefinedAt(succ) || cost < dest(succ)) {
            dest      += (succ->cost)
            mapToPred += (succ->pred)
          }
        }
        var nodeCnt = 0
        var canceled = false
        @tailrec def rec(pq: PriorityQueue[NodeWeight]) {
          if(pq.nonEmpty && (pq.head._1 ne potentialSuccessor)) { 
            val nodeWeight = pq.dequeue
            val node = nodeWeight._1
            if (!node.visited) {
              sortedAdjacentsNodes(node) match {
                case Some(ordNodes) =>
                  if (ordNodes.nonEmpty) pq ++= (ordNodes)
                  @tailrec def loop(pq2: PriorityQueue[NodeWeight]) {
                    if (pq2.nonEmpty) {
                      relax(node, pq2.dequeue._1)
                      loop(pq2)
                    }
                  }
                  loop(ordNodes)
                case None =>
              }
              node.visited = true
              if (doNodeVisitor && extendedVisitor.map { v =>
                nodeCnt += 1
                v(node, nodeCnt, 0,
                  new DijkstraInformer[NodeT,T] {
                    def queueIterator = qNodes.toIterator
                    def costsIterator = dest.toIterator
                  })
              }.getOrElse(nodeVisitor(node)) == Cancel) {
                canceled = true
                return
              }
            }
            rec(pq)
          }
        }
        rec(qNodes) 
        def traverseMapNodes(map: MutableMap[NodeT,NodeT]): Option[Path] = {
          map.get(potentialSuccessor).map ( _ =>
            new MinWeightEdgeLazyPath(
                new MapPathTraversable[NodeT](map, potentialSuccessor, root),
                subgraphEdges)
          ) orElse (
            if(root eq potentialSuccessor) Some(Path.zero(potentialSuccessor)) else None
          )
        }
        if (canceled) None
        else traverseMapNodes(mapToPred)
      }
    }
                      
    final def findCycle(implicit visitor: A => Unit = emptyVisitor): Option[Cycle] = {
      cycle( 
        new Traversal(Successors, subgraphNodes, subgraphEdges, nodeVisitor(visitor),
                      edgeVisitor(visitor), ordering).depthFirstSearchWGB(root),
        subgraphEdges
      )
    }
    
    final def depthFirstSearchWGB(globalState: Array[Handle]= Array.empty[Handle])(
                                  implicit visitor: A => Unit = emptyVisitor)
        : (Option[NodeT], Stack[CycleStackElem]) =
      newTraversal( parameters.direction, subgraphNodes, subgraphEdges,
                    nodeVisitor(visitor), edgeVisitor(visitor), ordering).
          depthFirstSearchWGB(root, globalState = globalState)
  }

  protected case class InnerNodeTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.InnerNodeTraverser
         with Impl[NodeT] {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => InnerNodeTraverser = copy _

    final protected def nodeVisitor[U](f: NodeT => U): (NodeT) => VisitorReturn = {
      @inline def legacy(n: NodeT): VisitorReturn = { f(n); Continue }
      chose(f, legacy)
    }
    final protected def edgeVisitor[U](f: NodeT => U): (EdgeT) => Unit = noEdgeAction
  }
  
  def innerNodeTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    InnerNodeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)

  protected case class OuterNodeTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.OuterNodeTraverser
         with Impl[N] {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => OuterNodeTraverser = copy _

    final protected def nodeVisitor[U](f: N => U): (NodeT) => VisitorReturn =
      (n: NodeT) => { f(n.value); Continue }

    final protected def edgeVisitor[U](f: N => U): (EdgeT) => Unit = noEdgeAction
  }

  def outerNodeTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    OuterNodeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
    
  protected case class InnerEdgeTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.InnerEdgeTraverser
         with Impl[EdgeT] {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => InnerEdgeTraverser = copy _

    final protected def nodeVisitor[U](f: EdgeT => U): (NodeT) => VisitorReturn = noNodeAction
    final protected def edgeVisitor[U](f: EdgeT => U): (EdgeT) => Unit = (e: EdgeT) => f(e)
  }
  
  def innerEdgeTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    InnerEdgeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
  
  protected case class OuterEdgeTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.OuterEdgeTraverser
         with Impl[E[N]] {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => OuterEdgeTraverser = copy _

    final protected def nodeVisitor[U](f: E[N] => U): (NodeT) => VisitorReturn = noNodeAction
    final protected def edgeVisitor[U](f: E[N] => U): (EdgeT) => Unit = (e: EdgeT) => f(e.toOuter)
  }
  
  def outerEdgeTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    OuterEdgeTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)
  
  protected case class InnerElemTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.InnerElemTraverser
         with Impl[InnerElem] {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => InnerElemTraverser = copy _
        
    final protected def nodeVisitor[U](f: InnerElem => U): (NodeT) => VisitorReturn = {
      @inline def legacy(n: NodeT): VisitorReturn = { f(n); Continue } 
      chose(f, legacy)
    }
    final protected def edgeVisitor[U](f: InnerElem => U): (EdgeT) => Unit = (e: EdgeT) => f(e)
  }
  
  def innerElemTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    InnerElemTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)

  protected case class OuterElemTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.OuterElemTraverser
         with Impl[OuterElem[N,E]] {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => OuterElemTraverser = copy _

    final protected def nodeVisitor[U](f: OuterElem[N,E] => U): (NodeT) => VisitorReturn =
      (n: NodeT) => { f(n.value); Continue }

    final protected def edgeVisitor[U](f: OuterElem[N,E] => U): (EdgeT) => Unit =
      (e: EdgeT) => f(e.toOuter.asInstanceOf[OuterEdge[N,E]])
  }

  def outerElemTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    OuterElemTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)

  protected trait DownUpTraverser[A] {
    this: Traverser[A,_] =>

    final protected def downUpForeach[U](down: NodeT => VisitorReturn, up  : NodeT => Unit): Unit =
      newTraversal( parameters.direction, subgraphNodes, subgraphEdges,
                    down, noEdgeAction, ordering).
      depthFirstSearch(root, maxDepth = parameters.maxDepth, nodeUpVisitor = up)

    final protected def edgeVisitor[U](f: (A) => U): (EdgeT) => Unit = noEdgeAction
  }

  protected case class InnerNodeDownUpTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.InnerNodeDownUpTraverser
         with DownUpTraverser[(Boolean, NodeT)]
         with Impl[(Boolean, NodeT)] {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => InnerNodeDownUpTraverser = copy _
        
    final override def foreach[U](f: ((Boolean, NodeT)) => U): Unit = downUpForeach(
        nodeVisitor(f),
        (n: NodeT) => f(false, n)
      )
    final protected def nodeVisitor[U](f: ((Boolean, NodeT)) => U): (NodeT) => VisitorReturn =
      (n: NodeT) => { f(true,  n); Continue } 
  }
  
  def innerNodeDownUpTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    InnerNodeDownUpTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)

  protected case class OuterNodeDownUpTraverser(
      override val root         : NodeT,
      override val parameters   : Parameters         = Parameters(),
      override val subgraphNodes: (NodeT) => Boolean = anyNode,
      override val subgraphEdges: (EdgeT) => Boolean = anyEdge,
      override val ordering     : ElemOrdering       = noOrdering)
      extends super.OuterNodeDownUpTraverser
         with DownUpTraverser[(Boolean, N)]
         with Impl[(Boolean, N)] {
    
    final protected def newTraverser:
        (NodeT, Parameters, (NodeT) => Boolean, (EdgeT) => Boolean, ElemOrdering)
        => OuterNodeDownUpTraverser = copy _

    final override def foreach[U](f: ((Boolean, N)) => U): Unit = downUpForeach(
        nodeVisitor(f),
        (n: NodeT) => f(false, n)
      )
    final protected def nodeVisitor[U](f: ((Boolean, N)) => U): (NodeT) => VisitorReturn =
      (n: NodeT) => { f(true,  n.value); Continue } 
  }
  
  def outerNodeDownUpTraverser(
      root         : NodeT,
      parameters   : Parameters         = Parameters(),
      subgraphNodes: (NodeT) => Boolean = anyNode,
      subgraphEdges: (EdgeT) => Boolean = anyEdge,
      ordering     : ElemOrdering       = noOrdering) =
    OuterNodeDownUpTraverser(root, parameters, subgraphNodes, subgraphEdges, ordering)

  class Traversal(direction  : Direction,
                  nodeFilter : (NodeT) => Boolean,
                  edgeFilter : (EdgeT) => Boolean,
                  nodeVisitor: (NodeT) => VisitorReturn,
                  edgeVisitor: (EdgeT) => Unit,
                  ordering   : ElemOrdering)
    extends super.Traversal(direction, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor, ordering)
  {
    final protected val addMethod = direction match {
      case Successors   => Node.addDiSuccessors _
      case Predecessors => Node.addDiPredecessors _
      case AnyConnected => Node.addNeighbors _
    }
    final protected val addFilteredMethod = direction match {
      case Successors   => filteredDiSuccessors _
      case Predecessors => filteredDiPredecessors _
      case AnyConnected => filteredNeighbors _
    }
    final protected val edgesMethod = direction match {
      case Successors   => (n: NodeT) => n.outgoing
      case Predecessors => (n: NodeT) => n.incoming
      case AnyConnected => (n: NodeT) => n.edges
    }
    @transient final protected val doNodeFilter = isCustomNodeFilter(nodeFilter)
    @transient final protected val doEdgeFilter = isCustomEdgeFilter(edgeFilter)
    @transient final protected val doFilter     = doNodeFilter || doEdgeFilter
    @transient final protected val doNodeVisitor= isCustomNodeVisitor(nodeVisitor)
    @transient final protected val doEdgeVisitor= isCustomEdgeVisitor(edgeVisitor)
    @transient final protected val (doNodeSort, nodeOrdering, reverseNodeOrdering,
                                    doEdgeSort, edgeOrdering, reverseEdgeOrdering) =
      ordering match {
        case nO: NodeOrdering => (true,  nO,   nO.reverse,
                                  false, null, null)
        case eO: EdgeOrdering => (false, null, null,
                                  true,  eO,   eO.reverse)
        case _ : NoOrdering   => (false, null, null,
                                  false, null, null)
      }
    protected[collection]
    def filteredDi(direction: Direction,
                   node     :  NodeT,
                   isVisited: (NodeT) => Boolean,
                   reverse  : Boolean): Iterable[NodeT] =
    {
      val edges: Iterable[EdgeT] = {
        val edges = {
          val es = edgesMethod(node)
          if (doEdgeFilter) es filter edgeFilter
          else              es
        }
        if(doEdgeSort) {
          def sorted(ordering: Ordering[EdgeT]) = edges match {
            case a: ArraySet[EdgeT] => a.sorted(ordering)
            case s                  => s.toList.sorted(ordering)
          }
          if(reverse) sorted(reverseEdgeOrdering)
          else        sorted(edgeOrdering)
        } else        edges
      }
      val succ = ArraySet.empty[NodeT](edges.size)
      if (doFilter) {
        def addFilteredNeighbors(edge: EdgeT) {
          addMethod(node, edge,
                   (n: NodeT) => if (nodeFilter(n) && ! isVisited(n))
                                 succ += n)
          if (doEdgeVisitor) edgeVisitor(edge)
        }
        edges foreach addFilteredNeighbors
      } else {
        edges foreach { (e: EdgeT) =>
          addMethod(node, e,
                    (n: NodeT) => if (! isVisited(n)) succ += n)
          if (doEdgeVisitor) edgeVisitor(e)
        }
      }
      if(doNodeSort)
        if(reverse) succ.sorted(reverseNodeOrdering)
        else        succ.sorted(nodeOrdering)
      else          succ
    }
    @inline final override protected[collection]
    def filteredDiSuccessors(node     :  NodeT,
                             isVisited: (NodeT) => Boolean,
                             reverse  : Boolean): Iterable[NodeT] =
      filteredDi(Successors, node, isVisited, reverse)
    @inline final override protected[collection]
    def filteredDiPredecessors(node     :  NodeT,
                               isVisited: (NodeT) => Boolean,
                               reverse  : Boolean): Iterable[NodeT] =
      filteredDi(Predecessors, node, isVisited, reverse)
    @inline final override protected[collection]
    def filteredNeighbors(node     :  NodeT,
                          isVisited: (NodeT) => Boolean,
                          reverse  : Boolean): Iterable[NodeT] =
      filteredDi(AnyConnected, node, isVisited, reverse)

    override def apply(root        : NodeT,
                       pred        : (NodeT) => Boolean = noNode,
                       breadthFirst: Boolean            = true,
                       maxDepth    : Int                = 0): Option[NodeT] =
    {
      if (breadthFirst) breadthFirstSearch(root, pred, maxDepth)
      else              depthFirstSearch  (root, pred, maxDepth)
    }
    override def depthFirstSearch(
        root         : NodeT,
        pred         : (NodeT) => Boolean = noNode,
        maxDepth     : Int                = 0,
        nodeUpVisitor: (NodeT) => Unit    = noNodeUpAction): Option[NodeT] =
      _depthFirstSearch(root, pred, maxDepth, nodeUpVisitor)._1

    final protected[GraphTraversalImpl] def _depthFirstSearch(
        root         : NodeT,
        pred         : (NodeT) => Boolean = noNode,
        maxDepth     : Int                = 0,
        nodeUpVisitor: (NodeT) => Unit    = noNodeUpAction): (Option[NodeT], Stack[(NodeT, Int)]) =
    withHandle() { implicit visitedHandle => 
      val stack: Stack[(NodeT, Int)] = Stack((root, 0))
      val path:  Stack[(NodeT, Int)] = Stack()
      val untilDepth: Int = if (maxDepth > 0) maxDepth else java.lang.Integer.MAX_VALUE
      @inline def isVisited(n: NodeT): Boolean = n.visited  
      val extendedVisitor = nodeVisitor match {
        case e: ExtendedNodeVisitor => Some(e)
        case _ => None
      }
      val doNodeUpVisitor = isCustomNodeUpVisitor(nodeUpVisitor)
      var res: Option[NodeT] = None
      var nodeCnt = 0
      root.visited = true
      @tailrec def loop {
        if(stack.nonEmpty) {
          val (current, depth) = {
            val popped = stack.pop
            val depth = popped._2
            if (depth > 0)
              while (path.head._2 >= depth) {
                if (doNodeUpVisitor) nodeUpVisitor(path.head._1)
                path.pop
              }
            path.push(popped)
            popped
          }
          if (doNodeVisitor && extendedVisitor.map{ v =>
                nodeCnt += 1
                v(current, nodeCnt, depth,
                  new DfsInformer[NodeT] {
                    def stackIterator = stack.toIterator 
                    def pathIterator  = path .toIterator 
                  })
              }.getOrElse(nodeVisitor(current)) == Cancel)
              return
          if (pred(current) && (current ne root)) {
            res = Some(current)
          } else {
            if (depth < untilDepth)
              for (n <- addFilteredMethod(current, isVisited, true)
                        filterNot (isVisited(_))) {
                stack.push((n, depth + 1))
                n.visited = true
              }
            loop
          }
        }
      }
      loop
      if (doNodeUpVisitor) path foreach (e => nodeUpVisitor(e._1))
      (res, path)
    }
    /**
     * Tail-recursive white-gray-black DFS implementation for cycle detection.
     * 
     * @param root start node for the search
     * @param predicate node predicate marking an end condition for the search
     */
    protected[collection]
    def depthFirstSearchWGB(root      :  NodeT,
                            predicate : (NodeT) => Boolean  = noNode,
                            globalState: Array[Handle]= Array.empty[Handle])
        : (Option[NodeT], Stack[CycleStackElem]) =
    {
      withHandles(2, globalState) { handles =>
        implicit val visitedHandle = handles(0)
        val blackHandle = handles(1)
        
        // (node, predecessor, exclude, 0 to 2 multi edges) with last two for undirected
        val stack: Stack[(NodeT, NodeT, Boolean, Iterable[EdgeT])] =
            Stack((root, root, false, Nil))
        // (node, connecting with prev)
        val path = Stack.empty[CycleStackElem]
        val isDiGraph = thisGraph.isDirected
        def isWhite (node: NodeT) = nonVisited(node)
        def isGray  (node: NodeT) = isVisited(node) && ! (node bit(blackHandle))
        def isBlack (node: NodeT) = node bit(blackHandle)
        def setGray (node: NodeT) { node.visited = true }
        def setBlack(node: NodeT) { node.bit_=(true)(blackHandle) } 
  
        def onNodeDown(node: NodeT) { setGray (node) } 
        def onNodeUp  (node: NodeT) { setBlack(node) }
  
        def isVisited (node: NodeT) = node.visited
        def nonVisited(node: NodeT) = ! isVisited(node)
        val extendedVisitor = nodeVisitor match {
          case e: ExtendedNodeVisitor => Some(e)
          case _ => None
        }
        var res: Option[NodeT] = None
        var nodeCnt = 0
        @tailrec def loop(pushed: Boolean) {
          if (res.isEmpty)
            if (stack.isEmpty)
              path foreach (t => setBlack(t._1))
            else {
              val popped = stack.pop
              if (! pushed)
                while ( path.nonEmpty &&
                       (path.head._1 ne root) &&
                       (path.head._1 ne popped._2)) {
                  val p = path.pop._1
                  if (! isBlack(p))
                    onNodeUp(p) 
                }
              val exclude: Option[NodeT] = if (popped._3) Some(popped._2) else None
              val current = popped._1
              path.push((current, popped._4))
              if (nonVisited(current)) onNodeDown(current)
              if (doNodeVisitor && extendedVisitor.map{ v =>
                    nodeCnt += 1
                    v(current, nodeCnt, 0,
                      new WgbInformer[NodeT, EdgeT] {
                        def stackIterator = stack.toIterator 
                        def pathIterator  = path .toIterator 
                      })
                  }.getOrElse(nodeVisitor(current)) == Cancel)
                return
              if (predicate(current) && (current ne root))
                res = Some(current)
              else {
                var pushed = false
                for (n <- addFilteredMethod(current, isBlack(_), true)
                          filterNot (isBlack(_))) { 
                  if (isGray(n)) {
                    if (exclude map (_ ne n) getOrElse true)
                      res = Some(n)
                  } else {
                    if (isDiGraph)
                      stack.push((n, current, false, Nil))
                    else {
                      val (excl, multi): (Boolean, Iterable[EdgeT]) = {
                        val conn = current connectionsWith n
                        (conn.size: @switch) match {
                          case 0 => throw new NoSuchElementException
                          case 1 => (true, conn)
                          case _ => (false, conn)
                        }
                      }
                      stack.push((n, current, excl, multi))
                    }
                    pushed = true
                  }
                }
                loop(pushed)
              }
            }
        }
        loop(true)
        (res, path)
      }
    }
    override def breadthFirstSearch(root    : NodeT,
                                    pred    : (NodeT) => Boolean = noNode,
                                    maxDepth: Int                = 0): Option[NodeT] =
    {
      withHandle() { implicit visitedHandle => 
        val untilDepth = if (maxDepth > 0) maxDepth else java.lang.Integer.MAX_VALUE
        var depth = 0
        var nodeCnt = 0
        val q = Queue[(NodeT, Int)](root -> depth)
        val doNodeVisitor = isCustomNodeVisitor(nodeVisitor)
        val extendedVisitor =
          if (doNodeVisitor)
            nodeVisitor match {
              case e: ExtendedNodeVisitor => Some(e)
              case _ => None
            }
          else None
        @inline def visited(n: NodeT) = n.visited  
        @inline def visitAndCanceled(n: NodeT) = {
          n.visited = true
          doNodeVisitor && extendedVisitor.map{ v =>
            nodeCnt += 1
            v(n, nodeCnt, depth,
              new BfsInformer[NodeT] {
                def queueIterator = q.toIterator 
              })
          }.getOrElse(nodeVisitor(n)) == Cancel
        }
        if (visitAndCanceled(root)) return None
        while (q.nonEmpty) { 
          val (prevNode, prevDepth) = q.dequeue
          if (prevDepth < untilDepth) {
            depth = prevDepth + 1
            for (node <- addFilteredMethod(prevNode, visited, false)) { 
              if (visitAndCanceled(node)) return None
              if (pred(node)) return Some(node)
              q enqueue (node -> depth)  
            }
          }
        }
        None
      }
    }
  }
  
  override def newTraversal(direction  : Direction                = Successors,
                            nodeFilter : (NodeT) => Boolean       = anyNode,
                            edgeFilter : (EdgeT) => Boolean       = anyEdge,
                            nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                            edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                            ordering   : ElemOrdering             = noOrdering) =    
    new Traversal(direction, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor, ordering)

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

    @inline override def last: T = enclosed(1) map toT getOrElse toT(s(0))
    def reverse: Traversable[T] = new Abstract.Traversable[T] {
      def foreach[U](f: T => U): Unit = {
        def fT(elem: S): Unit = f(toT(elem))
        def end(i: Int) = enclosed(i) map fT
        end(1)
        s foreach fT
        end(0)
      }
    }
    
    private lazy val upper = dropWhile map { pred =>
      var i = s.size - 1
      while (i >= 0 && pred(s(i)))
        i -= 1
      if (i < 0) 0 else i
    } getOrElse s.size
    
    lazy val source: Traversable[S] = new Abstract.Traversable[S] {
      def foreach[U](f: S => U): Unit = {
        enclosed(0) map f
        var i = upper
        var size = i
        while (i > 0) {
          i -= 1
          f(s(i))
        }
        enclosed(1) map f
        if (_size.isEmpty) _size = Some(size + enclosed.count(_.isDefined))
      }
    }
  }
  
  /** Enables lazy traversing of a `Map` with `key = source, value = target`.
   */
  final protected class MapPathTraversable[T](map: MutableMap[T,T], to: T, start: T)
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
  protected abstract class LazyPath(val nodes : Traversable[NodeT],
                                    edgeFilter: (EdgeT) => Boolean)
      extends Path {

    def foreach[U](f: OutParam[N,E] => U): Unit = {
      f(nodes.head)
      val edges = this.edges.toIterator
      for (n <- nodes.tail;
           e = edges.next) {
        f(e)
        f(n)
      }
    }
    def startNode = nodes.head
    def endNode   = nodes.last
    def isValid: Boolean = {
      nodes.headOption map { startNode =>
        val edges = this.edges.toIterator
        (nodes.head /: nodes.tail){ (prev: NodeT, n: NodeT) =>
          if (edges.hasNext) {
            val e = edges.next
            if (! e.matches((x: NodeT) => x eq prev,
                            (x: NodeT) => x eq n   )) return false
            n
          } else return false
        }
        true
      } getOrElse false
    }

    override def equals(other: Any) = other match {
      case that: GraphTraversalImpl[N,E]#Path => 
        (this eq that) ||
        that.toArray[OutParam[N,E]].sameElements(toArray[OutParam[N,E]])
      case _ => false
    }
    override def hashCode = nodes.## + 27 * edges.##  
  }
  
  /** `LazyPath` with deferred edges selection.
   */
  protected abstract class SimpleLazyPath(override val nodes : Traversable[NodeT],
                                          edgeFilter: (EdgeT) => Boolean)
      extends LazyPath(nodes, edgeFilter) {
    
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
  protected class AnyEdgeLazyPath(override val nodes : Traversable[NodeT],
                                  edgeFilter: (EdgeT) => Boolean)
      extends SimpleLazyPath(nodes, edgeFilter) {
    
    final protected def selectEdge(from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        (from outgoingTo to find edgeFilter).get
      else
        (from findOutgoingTo to).get
  }

  /** `LazyPath` with edges selected by minimal weight.
   */
  protected class MinWeightEdgeLazyPath(override val nodes : Traversable[NodeT],
                                        edgeFilter: (EdgeT) => Boolean)
      extends SimpleLazyPath(nodes, edgeFilter) {
    
    final def selectEdge (from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        from outgoingTo to filter edgeFilter min(Edge.WeightOrdering)
      else
        from outgoingTo to min(Edge.WeightOrdering)
  }

  /** `LazyPath` with edge selection such that there exists no duplicate edge in the path.
   */
  protected class MultiEdgeLazyPath(
      override val nodes : ReverseStackTraversable[CycleStackElem, NodeT],
      edgeFilter: (EdgeT) => Boolean)
      extends LazyPath(nodes, edgeFilter) {
    
    import mutable.EqSet, mutable.EqSet.EqSetMethods
    final protected val multi = EqSet[EdgeT](graphSize / 2) 
        
    final lazy val edges = {
      val buf = new ArrayBuffer[EdgeT](nodes.size) {
        override def stringPrefix = "Edges"
      }
      val isDiGraph = thisGraph.isDirected
      (nodes.head /: nodes.source.tail){ (prev: NodeT, elem: CycleStackElem) =>
        val (n, conn) = elem
        def get(edges: Iterable[EdgeT], pred: (EdgeT) => Boolean) = {
          def ok(e: EdgeT): Boolean = ! multi.contains(e) && edgeFilter(e) && pred(e) 
          if (isDiGraph)
            (edges find ok).get
          else {
            val (di, unDi) = edges filter ok partition (_.isDirected)
            di.headOption getOrElse (unDi.head) 
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
  
  protected class AnyEdgeLazyCycle(override val nodes : Traversable[NodeT],
      edgeFilter: (EdgeT) => Boolean)
      extends AnyEdgeLazyPath(nodes, edgeFilter)
         with Cycle    

  protected class MultiEdgeLazyCycle(
      override val nodes : ReverseStackTraversable[CycleStackElem, NodeT],
      edgeFilter: (EdgeT) => Boolean)
      extends MultiEdgeLazyPath(nodes, edgeFilter)
         with Cycle    
}
object GraphTraversalImpl {
  import GraphTraversal._

  /** Extended node visitor informer for depth first searches. 
   */
  trait DfsInformer[N] extends NodeInformer {
    import DfsInformer._
    def stackIterator: DfsStack[N]
    def pathIterator:  DfsPath [N]
  }
  object DfsInformer {
    type DfsStack[N] = Iterator[(N, Int)]
    type DfsPath [N] = Iterator[(N, Int)]
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
    type WgbStack[N,E] = Iterator[(N, N, Boolean, Iterable[E])]
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
    type BfsQueue[N] = Iterator[(N, Int)]
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
    type DijkstraQueue[N,T] = Iterator[(N,T)]
    type DijkstraCosts[N,T] = Iterator[(N,T)]
    def unapply[N, T: Numeric](inf: DijkstraInformer[N,T])
        : Option[(DijkstraQueue[N,T], DijkstraCosts[N,T])] =
      Some(inf.queueIterator, inf.costsIterator)
  }
} 