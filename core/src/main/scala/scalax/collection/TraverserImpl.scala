package scalax.collection

import scala.language.higherKinds
import scala.annotation.{switch, tailrec}
import scala.collection.generic.{FilterMonadic, Growable}
import scala.collection.mutable.{ArrayBuffer, ArrayStack => Stack, Queue, PriorityQueue, Map => MMap}
import scala.math.abs

import scala.collection.FilterableSet
import scala.collection.FilteredSet
import scalax.collection.GraphPredef.{OuterNode, EdgeLikeIn}
import immutable.SortedArraySet
import mutable.{ArraySet, EqHashSet}
import scalax.collection.GraphEdge.DiEdge

/** Default implementation of the graph algorithms to maintain the functionality
 *  defined by [[GraphTraversal]].
 *   
 *  @author Peter Empen
 */
trait TraverserImpl[N, E[X] <: EdgeLikeIn[X]] {
  thisGraph: GraphTraversalImpl[N,E] =>

  import GraphTraversalImpl._
  import GraphTraversal._
  import Visitor._
  import State._
    
  protected[collection] trait Impl[A, +This <: Traverser[A,This] with Impl[A,This]]
      extends Traverser[A,This] {
    thisImpl: This =>

    final protected def apply[U](pred: NodeFilter = noNode, visitor: A => U = empty): Option[NodeT] =
      new Runner[U](pred, visitor)()

    final def findCycle[U](implicit visitor: A => U = empty): Option[Cycle] = {
      cycle(
        withParameters(Parameters(direction = Successors)).Runner(noNode, visitor).dfsWGB(),
        subgraphEdges
      )
    }
    
    final def pathUntil[U](pred: NodeFilter)(implicit visitor: A => U = empty): Option[Path] = {
      val (target, path) = 
          withParameters(parameters.withDirection(Successors)).Runner[U](pred, visitor).dfsStack()
      target map { _ =>
        new AnyEdgeLazyPath(
          new ReverseStackTraversable[DfsInformer.Element[NodeT], NodeT]
              (path, (e: DfsInformer.Element[NodeT]) => e.node),
          subgraphEdges)
      }
    }
    
    final def topologicalSort[U](ignorePredecessors: Boolean = false)
                                (implicit visitor: InnerElem => U = empty): CycleNodeOrTopologicalOrder = {
      val predecessors: MSet[NodeT] =
          if (ignorePredecessors)
            innerNodeTraverser(root, Parameters.Dfs(Predecessors)).to[MSet] -= root
          else MSet.empty
      def ignore(n: NodeT): Boolean = if (ignorePredecessors) predecessors contains n else false
      val (startNodes, inDegrees) =
        forInDegrees(
          innerNodeTraverser(root, Parameters.Dfs(AnyConnected),
                             n => subgraphNodes(n), subgraphEdges),
          includeInDegree = if (ignorePredecessors) ! ignore(_) else anyNode,
          includeAnyway = if (ignorePredecessors) Some(root) else None
        )
      Runner(noNode, empty).topologicalSort(startNodes -- predecessors, inDegrees)
    }

    final def shortestPathTo[T:Numeric, U](potentialSuccessor: NodeT,
                                           weight            : EdgeT => T,
                                           visitor           : A => U): Option[Path] =
      new Runner(noNode, visitor).shortestPathTo(potentialSuccessor, weight)

    /** Contains algorithms and local values to be used by the algorithms.
     *  Last target reusability and best possible run-time performance.
     *
     *  @param stopAt node predicate marking an end condition for the search
     */
    protected final class Runner[U](stopAt: NodeFilter, visitor: A => U) {

      private[this] val addMethod = parameters.direction match {
        case Successors   => Node.addDiSuccessors _
        case Predecessors => Node.addDiPredecessors _
        case AnyConnected => Node.addNeighbors _
      }
  
      private[this] val doNodeFilter = isCustomNodeFilter(subgraphNodes)
      private[this] val doEdgeFilter = isCustomEdgeFilter(subgraphEdges) || maxWeight.isDefined
      private[this] val (doNodeSort, nodeOrdering, reverseNodeOrdering,
                           doEdgeSort, edgeOrdering, reverseEdgeOrdering) =
        ordering match {
          case nO: NodeOrdering    => (true,  nO,   nO.reverse,
                                       false, null, null)
          case eO: EdgeOrdering    => (false, null, null,
                                       true,  eO,   eO.reverse)
          case _ : NoOrdering.type => (false, null, null,
                                       false, null, null)
        }
  
      /* doNodeVisitor:  whether any node visitor is to be called
       * nodeVisitor:    the simple node visitor or empty
       * extNodeVisitor: the extended node visitor or null
       * edgeVisitor:    the edge visitor or empty
       */
      private[this] val (doNodeVisitor, nodeVisitor, extNodeVisitor, edgeVisitor):
                        (Boolean,       NodeT => U,  ExtendedNodeVisitor[U], EdgeT => U) = {
        val nodeVisitor = thisImpl.nodeVisitor(visitor)
        val extNodeVisitor = visitor match {
            case ext: ExtendedNodeVisitor[U] => ext
            case _ => null
        }
        ( isDefined(nodeVisitor),
          if (extNodeVisitor eq null) nodeVisitor else empty[NodeT,U],
          extNodeVisitor,
          thisImpl.edgeVisitor(visitor)
        )
      }
      
      private[this] val filteredNodes = parameters.direction match {
        case Successors   => filteredSuccessors _
        case Predecessors => filteredPredecessors _
        case AnyConnected => filteredNeighbors _
      }

      private[this] def directionEdges(n: NodeT): NodeT => Set[EdgeT] with FilterableSet[EdgeT] = parameters.direction match {
        case Successors   => n outgoingTo _
        case Predecessors => n incomingFrom _
        case AnyConnected => n connectionsWith _
      }
      
      @inline protected[Impl] def apply(): Option[NodeT] =
        if (parameters.kind.isBsf) bfs() else dfs()
      
      @inline private[this] def estimatedNrOfNodes(node: NodeT) = {
        val max = node.edges.size
        if (thisGraph.isHyper) max * 4 else max
      }
      
      def maxDepth = if (parameters.maxDepth > 0) parameters.maxDepth
                     else java.lang.Integer.MAX_VALUE
      
      private[this] def sorted[A <: InnerElem with B, B <: InnerElem: reflect.ClassTag](
          set:      FilterMonadic[A,AnySet[A]],
          maxOrEst: Int, // maximum size of set or negative for an estimate
          ordering: Ordering[A]): AnySet[A] =
        set match {
          case a: ArraySet[A] => a.sorted(ordering)
          case t =>
            @inline def newArray(len: Int): Array[A] = new Array[B](len).asInstanceOf[Array[A]]
            val size = abs(maxOrEst)
            var cnt = 0
            val arr =
              if (maxOrEst >= 0) {
                val arr = newArray(maxOrEst)
                t foreach { a => arr(cnt) = a; cnt += 1 }
                if (maxOrEst > cnt) {
                  val shrinked = newArray(cnt)
                  Array.copy(arr, 0, shrinked, 0, cnt)
                  shrinked
                } else arr
              } else {
                val buf = new ArrayBuffer[A](maxOrEst)
                t foreach { a => buf += a; cnt += 1 }
                val arr = newArray(cnt)
                buf copyToArray arr
                arr
              }
            new SortedArraySet[A](arr)(ordering)
        }
  
      @inline private[this] def sortedNodes(nodes: AnySet[NodeT], maxOrEst: Int, reverse: Boolean): AnySet[NodeT] =
        sorted[NodeT,InnerElem](
            nodes, maxOrEst, if (reverse) reverseNodeOrdering else nodeOrdering)
      
      private[this] def filtered(
          node:      NodeT,
          isVisited: NodeFilter,
          _edges:    FilterMonadic[EdgeT,AnySet[EdgeT]], // already filtered if adequate
          reverse:   Boolean): Traversable[NodeT] = {
        
        val edges = {
          if(doEdgeSort) {
            val maxEdges = node.edges.size
            if(reverse) sorted[EdgeT,InnerElem](_edges, maxEdges, reverseEdgeOrdering)
            else        sorted[EdgeT,InnerElem](_edges, maxEdges, edgeOrdering)
          } else _edges
        }

        val filter = chooseFilter(isVisited)
        val doEdgeVisitor = isDefined(edgeVisitor)
        val estimatedNodes = estimatedNrOfNodes(node)
        def withEdges(withNode: NodeT => Unit) =
          edges foreach { e =>
            addMethod(node, e, withNode)
            if (doEdgeVisitor) edgeVisitor(e)
          }
        if (doEdgeSort) {
          /* The node set to be returned must reflect edge ordering.
           * doEdgeSort and doNodeSort are mutually exclusive.
           */
          val set = new EqHashSet[NodeT](estimatedNodes)
          val succ = new ArrayBuffer[NodeT](estimatedNodes)
          withEdges( n =>
            if (filter(n) && set.add(n)) succ += n
          )
          succ
        } else {
          val succ = new EqHashSet[NodeT](estimatedNodes)
          withEdges( n =>
            if (filter(n)) succ += n
          )
          if(doNodeSort) sortedNodes(succ, succ.size, reverse)
          else succ
        }
      }
      
      private[this] val withEdgeFiltering: Boolean =
        doEdgeFilter || doEdgeSort || isDefined(edgeVisitor) || maxWeight.isDefined
      
      @inline private[this] def chooseFilter(isVisited: NodeFilter): NodeFilter =
        if (doNodeFilter) (n: NodeT) => ! isVisited(n) && subgraphNodes(n)
        else              (n: NodeT) => ! isVisited(n)
        
      private[this] def filtered(
          nodes:     AnySet[NodeT],
          maxNodes:  Int,                             
          isVisited: NodeFilter,
          reverse:   Boolean): AnySet[NodeT] = {
        val filtered = new FilteredSet(nodes, chooseFilter(isVisited))
        if(doNodeSort) sortedNodes(filtered, maxNodes, reverse)
        else filtered
      }

      private[this] def edgeFilter(cumWeight: Double): EdgeFilter = maxWeight.fold(
        ifEmpty = subgraphEdges)(
        w => {
          def weightFilter: EdgeFilter = e => cumWeight + w.edgeWeight(e) <= w.value
          if (isCustomEdgeFilter(subgraphEdges)) e => subgraphEdges(e) && weightFilter(e)
          else weightFilter
        })
        
      private[this] def minWeight(n: NodeT, neighbor: NodeT, cumWeight: Double): Double = maxWeight.fold[Double](
        ifEmpty = throw new MatchError("maxWeight is expected to be defined."))(
        w => w.edgeWeight(directionEdges(n)(neighbor) withFilter edgeFilter(cumWeight) min w.ordering)
      )

      final private[this] def filteredEdges(edges: AnySet[EdgeT], cumWeight: Double): FilterMonadic[EdgeT,AnySet[EdgeT]] =
        if (doEdgeFilter) edges withFilter edgeFilter(cumWeight)
        else edges
        
      private[this] def filteredSuccessors(
          node:        NodeT,
          isVisited:   NodeFilter,
          cumWeight:   Double,
          reverse:     Boolean): Traversable[NodeT] = {
  
        if (withEdgeFiltering)
          filtered(node, isVisited, filteredEdges(node.outgoing, cumWeight), reverse)
        else {
          val succ = node.diSuccessors
          filtered(succ, succ.size, isVisited, reverse)
        }
      }
        
      private[this] def filteredPredecessors(
          node:        NodeT,
          isVisited:   NodeFilter,
          cumWeight:   Double,
          reverse:     Boolean): Traversable[NodeT] = {
        
        if (withEdgeFiltering)
          filtered(node, isVisited, filteredEdges(node.incoming, cumWeight), reverse)
        else
          filtered(node.diPredecessors, - estimatedNrOfNodes(node), isVisited, reverse)
      }
        
      private[this] def filteredNeighbors(
          node:        NodeT,
          isVisited:   NodeFilter,
          cumWeight:   Double,
          reverse:     Boolean): Traversable[NodeT] = {
            
        if (withEdgeFiltering)
          filtered(node, isVisited, filteredEdges(node.edges, cumWeight), reverse)
        else
          filtered(node.neighbors, - estimatedNrOfNodes(node), isVisited, reverse)
      }
  
      protected[collection] def shortestPathTo[T:Numeric](
          potentialSuccessor: NodeT,
          weight            : EdgeT => T): Option[Path] = {
        withHandle() { implicit visitedHandle => 
          val num = implicitly[Numeric[T]] 
          import num._
          type PrioQueueElem = DijkstraInformer.Element[NodeT,T]
          val  PrioQueueElem = DijkstraInformer.Element
          
          @inline def visited(n: NodeT) = n.visited
          val untilDepth: Int = maxDepth
          val edgeOrdering = Edge.weightOrdering(weight)
          val dest      = MMap[NodeT,T](root -> zero)
          val mapToPred = MMap[NodeT,NodeT]()
          // not implicit due to issues #4405 and #4407
          object nodeOrdering extends Ordering[PrioQueueElem] {
            def compare(x: PrioQueueElem, y: PrioQueueElem) = num.compare(y.cumWeight, x.cumWeight)
          }
          val qNodes = new PriorityQueue[PrioQueueElem]()(nodeOrdering) += PrioQueueElem(root, zero, 0)
  
          def sortedAdjacentNodes(node: NodeT, depth: Depth): PriorityQueue[PrioQueueElem] = {
            val predecessorWeight = dest(node)
            filteredSuccessors(node, visited, Double.NaN, false).foldLeft(
              new PriorityQueue[PrioQueueElem]()(nodeOrdering))( 
              (q,n) => q += PrioQueueElem(
                  n,
                  predecessorWeight + weight(node.outgoingTo(n).withFilter(subgraphEdges(_)).min(edgeOrdering)),
                  depth)
            )
          }
          def relax(pred: NodeT, succ: NodeT) {
            val cost = dest(pred) +
                       weight(pred.outgoingTo(succ).withFilter(subgraphEdges(_)).min(edgeOrdering))
            if(! dest.isDefinedAt(succ) || cost < dest(succ)) {
              dest      += succ -> cost
              mapToPred += succ -> pred
            }
          }
          var nodeCnt = 0
          @tailrec def rec(pq: PriorityQueue[PrioQueueElem]) {
            if (pq.nonEmpty && (pq.head.node ne potentialSuccessor)) { 
              val PrioQueueElem(node,_,depth) = pq.dequeue
              if (! node.visited) {
                val ordNodes =
                  if (depth == untilDepth) PriorityQueue.empty[PrioQueueElem](nodeOrdering)
                  else sortedAdjacentNodes(node, depth + 1)
                pq ++= ordNodes
                
                @tailrec def loop(pq2: PriorityQueue[PrioQueueElem]) {
                  if (pq2.nonEmpty) {
                    relax(node, pq2.dequeue.node)
                    loop(pq2)
                  }
                }
                loop(ordNodes)
                
                node.visited = true
                if (doNodeVisitor)
                  if (isDefined(nodeVisitor)) nodeVisitor(node)
                  else {
                    nodeCnt += 1
                    extNodeVisitor(node, nodeCnt, 0,
                      new DijkstraInformer[NodeT,T] {
                        def queueIterator = qNodes.iterator
                        def costsIterator = dest.iterator
                      }
                    )
                  }
              }
              rec(pq)
            }
          }
          rec(qNodes) 
          def traverseMapNodes(map: MMap[NodeT,NodeT]): Option[Path] = {
            map.get(potentialSuccessor).map ( _ =>
              new MinWeightEdgeLazyPath(
                  new MapPathTraversable[NodeT](map, potentialSuccessor, root),
                  subgraphEdges,
                  Edge.weightOrdering(weight))
            ) orElse (
              if(root eq potentialSuccessor) Some(Path.zero(potentialSuccessor)) else None
            )
          }
          traverseMapNodes(mapToPred)
        }
      }
                        
      protected[collection] def bfs(maybeHandle: Option[Handle] = None): Option[NodeT] = {
        withHandle(maybeHandle) { implicit visitedHandle =>
          val untilDepth: Int = maxDepth
          var depth = 0
          var nodeCnt = 0
          import BfsInformer.Element
          val q = Queue[Element[NodeT]](Element(root, depth))
          @inline def visited(n: NodeT) = n.visited  
          def visit(n: NodeT): Unit = {
            n.visited = true
            if (doNodeVisitor)
              if (isDefined(nodeVisitor)) nodeVisitor(n)
              else {
                nodeCnt += 1
                extNodeVisitor(n, nodeCnt, depth,
                  new BfsInformer[NodeT] {
                    def queueIterator = q.iterator 
                  }
                )
              }
          }
          visit(root)
          while (q.nonEmpty) { 
            val Element(prevNode, prevDepth, cumWeight) = q.dequeue
            if (prevDepth < untilDepth) {
              depth = prevDepth + 1
              for (n <- filteredNodes(prevNode, visited, cumWeight, false)) { 
                visit(n)
                if (stopAt(n)) return Some(n)
                q enqueue Element(
                  n,
                  depth,
                  maxWeight.fold(ifEmpty = 0d)(w => cumWeight + minWeight(prevNode, n, cumWeight))
                )  
              }
            }
          }
          None
        }
      }
  
      @inline protected[collection]
      def dfs[U](maybeHandle: Option[Handle] = None): Option[NodeT] = dfsStack(empty, maybeHandle)._1

      /** @return (node stopped at, stack of ...) */
      protected[collection]
      def dfsStack[U](nodeUpVisitor: (NodeT) => U = empty,
                      maybeHandle: Option[Handle] = None): (Option[NodeT], Stack[DfsInformer.Element[NodeT]]) =
        withHandle(maybeHandle) { implicit visitedHandle =>
          val untilDepth: Int = maxDepth
          val doNodeUpVisitor = isDefined(nodeUpVisitor)

          @inline def isVisited(n: NodeT): Boolean = n.visited
          import DfsInformer.Element
          val stack: Stack[Element[NodeT]] = Stack(Element(root, 0))
          val path:  Stack[Element[NodeT]] = Stack()
          var res: Option[NodeT] = None
          var nodeCnt = 0
          root.visited = true
          @tailrec def loop {
            if(stack.nonEmpty) {
              val Element(current, depth, cumWeight) = {
                val popped = stack.pop
                val depth = popped.depth
                if (depth > 0)
                  while (path.head.depth >= depth) {
                    if (doNodeUpVisitor) nodeUpVisitor(path.head.node)
                    path.pop
                  }
                path.push(popped)
                popped
              }
              if (doNodeVisitor)
                if (isDefined(nodeVisitor)) nodeVisitor(current)
                else {
                  nodeCnt += 1
                  extNodeVisitor(current, nodeCnt, depth,
                    new DfsInformer[NodeT] {
                      def stackIterator = stack.iterator
                      def pathIterator  = path .iterator
                    }
                  )
                }
              if (stopAt(current) && (current ne root)) {
                res = Some(current)
              } else {
                if (depth < untilDepth)
                  for (n <- filteredNodes(current, isVisited, cumWeight, true)) {
                    stack push Element(
                      n,
                      depth + 1,
                      maxWeight.fold(ifEmpty = 0d)(w => cumWeight + minWeight(current, n, cumWeight))
                    )
                    n.visited = true
                  }
                loop
              }
            }
          }
          loop
          if (doNodeUpVisitor) path foreach (e => nodeUpVisitor(e.node))
          (res, path)
        }
  
      /** Tail-recursive white-gray-black DFS implementation for cycle detection.
       */
      protected[collection]
      def dfsWGB(globalState: Array[Handle] = Array.empty[Handle]): (Option[NodeT], Stack[CycleStackElem]) = {
        withHandles(2, globalState) { handles =>
          implicit val visitedHandle = handles(0)
          val blackHandle = handles(1)
          
          import WgbInformer.Element
          val stack: Stack[Element[NodeT,EdgeT]] = Stack(Element(root, root, false, Nil))
          val path = Stack.empty[CycleStackElem] // (node, connecting with prev)
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
          var res: Option[NodeT] = None
          var nodeCnt = 0
          @tailrec def loop(pushed: Boolean) {
            if (res.isEmpty)
              if (stack.isEmpty)
                path foreach (t => setBlack(t._1))
              else {
                val Element(poppedNode, poppedPredecessor, poppedExclude, poppedMultiEdges, cumWeight) = stack.pop
                if (! pushed)
                  while ( path.nonEmpty &&
                         (path.head._1 ne root) &&
                         (path.head._1 ne poppedPredecessor)) {
                    val p = path.pop._1
                    if (! isBlack(p))
                      onNodeUp(p) 
                  }
                val exclude: Option[NodeT] = if (poppedExclude) Some(poppedPredecessor) else None
                val current = poppedNode
                path.push((current, poppedMultiEdges))
                if (nonVisited(current)) onNodeDown(current)
                if (doNodeVisitor)
                  if (isDefined(nodeVisitor)) nodeVisitor(current)
                  else {
                    nodeCnt += 1
                    extNodeVisitor(current, nodeCnt, 0,
                        new WgbInformer[NodeT, EdgeT] {
                          def stackIterator = stack.iterator 
                          def pathIterator  = path .iterator 
                      }
                    )
                  }
                if (stopAt(current) && (current ne root))
                  res = Some(current)
                else {
                  var pushed = false
                  for (n <- filteredNodes(current, isBlack(_), cumWeight, true)
                            withFilter (! isBlack(_))) { 
                    if (isGray(n)) {
                      if (exclude.fold(ifEmpty = true)(_ ne n))
                        res = Some(n)
                    } else {
                      val newCumWeight = maxWeight.fold(ifEmpty = 0d)(w => cumWeight + minWeight(current, n, cumWeight))
                      if (isDiGraph)
                        stack.push(Element(n, current, false, Nil, newCumWeight))
                      else {
                        val (excl, multi): (Boolean, Iterable[EdgeT]) = {
                          val conn = current connectionsWith n
                          (conn.size: @switch) match {
                            case 0 => throw new NoSuchElementException
                            case 1 => (true, conn)
                            case _ => (false, conn)
                          }
                        }
                        stack.push(Element(n, current, excl, multi, newCumWeight))
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

      protected[collection] def topologicalSort(
          layer_0: Traversable[NodeT],
          inDegrees: MMap[NodeT,Int],
          maybeHandle: Option[Handle] = None): CycleNodeOrTopologicalOrder =
        withHandle(maybeHandle) { implicit handle =>
          val untilDepth: Int = maxDepth
          val estimatedLayers: Int = expectedMaxNodes(4)
          val estimatedNodesPerLayer: Int = order / estimatedLayers
          val layers = new ArrayBuffer[Layer](estimatedLayers)
          def emptyBuffer: ArrayBuffer[NodeT] = new ArrayBuffer[NodeT](estimatedNodesPerLayer) 
          
          @tailrec def loop(layer: Int, layerNodes: ArrayBuffer[NodeT]): CycleNodeOrTopologicalOrder = {
            layers += Layer(layer, layerNodes)
            val currentLayerNodes = if (doNodeSort) layerNodes.sorted(nodeOrdering) else layerNodes
            val nextLayerNodes = emptyBuffer
            var maybeCycleNode: Option[NodeT] = None
            val nrEnqueued = (0 /: currentLayerNodes) { (i, node) =>
              if (doNodeVisitor) nodeVisitor(node)
              node.visited = true
              (0 /: filteredSuccessors(node, _.visited, Double.NaN, true)) { (nrZeroInDegree, n) =>
                val newInDegree = inDegrees(n) - 1
                inDegrees.update (n, newInDegree)
                if (newInDegree == 0) {
                  nextLayerNodes += n
                  nrZeroInDegree + 1
                } else {
                  maybeCycleNode = maybeCycleNode orElse Some(n)
                  nrZeroInDegree
                }
              } + i
            }
            def resultOnSuccess: CycleNodeOrTopologicalOrder = Right(new TopologicalOrder(layers, identity)) 
            if (nrEnqueued == 0)
              maybeCycleNode.fold(ifEmpty = resultOnSuccess)(cycleNode => Left(cycleNode))
            else if (layers.size == untilDepth)
              resultOnSuccess
            else
              loop(layer + 1, nextLayerNodes)
          }
          
          val startBuffer = layer_0 match {
            case b: ArrayBuffer[NodeT] => b
            case t                     => emptyBuffer ++ t
          }
          loop(0, startBuffer)
        }
      }

    protected[collection] object Runner {
      @inline final def apply[U](stopAt:  NodeFilter,
                                 visitor: A => U): Runner[U] = new Runner[U](stopAt, visitor)
    }
  }
}
