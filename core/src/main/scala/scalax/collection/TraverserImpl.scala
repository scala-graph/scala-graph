package scalax.collection

import scala.language.higherKinds
import scala.annotation.{switch, tailrec}
import scala.collection.generic.FilterMonadic
import scala.collection.mutable.{ArrayBuffer, ArrayStack => Stack,
                                 Queue, PriorityQueue, Map => MMap}
import scala.math.abs

import collection.FilteredSet
import GraphPredef.EdgeLikeIn
import immutable.SortedArraySet
import mutable.{ArraySet, EqSet}

trait TraverserImpl[N, E[X] <: EdgeLikeIn[X]] {
  thisGraph: GraphTraversalImpl[N,E] =>

  import GraphTraversalImpl._
  import GraphTraversal._
  import Visitor._
  import State._
    
  protected[collection] trait Impl[A, +This <: Traverser[A,This] with Impl[A,This]]
      extends Traverser[A,This] {
    thisImpl: This =>

    final protected val addMethod = parameters.direction match {
      case Successors   => Node.addDiSuccessors _
      case Predecessors => Node.addDiPredecessors _
      case AnyConnected => Node.addNeighbors _
    }

    final protected val doNodeFilter = isCustomNodeFilter(subgraphNodes)
    final protected val doEdgeFilter = isCustomEdgeFilter(subgraphEdges)
    final protected val (doNodeSort, nodeOrdering, reverseNodeOrdering,
                                    doEdgeSort, edgeOrdering, reverseEdgeOrdering) =
      ordering match {
        case nO: NodeOrdering => (true,  nO,   nO.reverse,
                                  false, null, null)
        case eO: EdgeOrdering => (false, null, null,
                                  true,  eO,   eO.reverse)
        case _ : NoOrdering   => (false, null, null,
                                  false, null, null)
      }

    final protected def apply[U](pred:    (NodeT) => Boolean = noNode,
                                 visitor: A => U             = empty): Option[NodeT] =
      new Runner[U](pred, visitor)()

    final def findCycle[U](implicit visitor: A => U = empty): Option[Cycle] = {
      cycle(
        withParameters(Parameters(direction = Successors)).Runner(noNode, visitor).dfsWGB(),
        subgraphEdges
      )
    }
    
    final def pathUntil[U](pred: (NodeT) => Boolean)
                          (implicit visitor: A => U = empty): Option[Path] = {
      val (target, path) = 
          withParameters(parameters.withDirection(Successors)).
          Runner[U](pred, visitor).dfs(empty)
      target map { _ =>
        new AnyEdgeLazyPath(
          new ReverseStackTraversable[(NodeT, Int), NodeT]
              (path, (elem: (NodeT, Int)) => elem._1),
          subgraphEdges)
      }
    }
    
    final def shortestPathTo[T:Numeric, U](potentialSuccessor: NodeT,
                                           weight            : EdgeT => T,
                                           visitor           : A => U): Option[Path] =
      new Runner(noNode, visitor).shortestPathTo(potentialSuccessor, weight)
      
    protected class Runner[U](stopAt:  (NodeT) => Boolean, visitor: A => U) {

      /* doNodeVisitor:  whether any node visitor is to be called
       * nodeVisitor:    the simple node visitor or empty
       * extNodeVisitor: the extended node visitor or null
       * edgeVisitor:    the edge visitor or empty
       */
      final protected[this]
      val (doNodeVisitor, nodeVisitor, extNodeVisitor,         edgeVisitor):
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
      
      final protected[this] val filteredNodes = parameters.direction match {
        case Successors   => filteredSuccessors _
        case Predecessors => filteredPredecessors _
        case AnyConnected => filteredNeighbors _
      }

      @inline final protected[Impl] def apply() =
        if (parameters.kind.isBsf) bfs else dfsNode(empty)
      
      @inline final private[this] def estimatedNrOfNodes(node: NodeT) = {
        val max = node.edges.size
        if (thisGraph.isHyper) max * 4 else max
      }
      
      final private[this]
      def sorted[A <: InnerElem with B, B <: InnerElem: reflect.ClassTag](
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
  
      @inline final private[this]
      def sortedNodes(nodes: AnySet[NodeT],
                      maxOrEst: Int,
                      reverse: Boolean): AnySet[NodeT] = 
        sorted[NodeT,InnerElem](
            nodes, maxOrEst, if (reverse) reverseNodeOrdering else nodeOrdering)
      
      final private[this] def filtered[U](
          node:        NodeT,
          isVisited:   (NodeT) => Boolean,
          _edges:      FilterMonadic[EdgeT,AnySet[EdgeT]], // already filtered if adequate
          edgeVisitor: EdgeT => U,
          reverse:     Boolean): Traversable[NodeT] = {
        
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
        import EqSet.EqSetMethods
        if (doEdgeSort) {
          /* The node set to be returned must reflect edge ordering.
           * doEdgeSort and doNodeSort are mutually exclusive.
           */
          val set = EqSet[NodeT](estimatedNodes)
          val succ = new ArrayBuffer[NodeT](estimatedNodes)
          withEdges( n =>
            if (filter(n) && set.add(n)) succ += n
          )
          succ
        } else {
          val succ = EqSet[NodeT](estimatedNodes)
          withEdges( n =>
            if (filter(n)) succ += n
          )
          val succSet = succ.toKeySet
          if(doNodeSort) sortedNodes(succSet, succ.size, reverse)
          else succSet
        }
      }
      
      @inline final private[this]
      def withEdgeFiltering[U](edgeVisitor: EdgeT => U): Boolean =
        doEdgeFilter || doEdgeSort || isDefined(edgeVisitor)
      
      @inline final private[this]
      def chooseFilter(isVisited: (NodeT) => Boolean): NodeT => Boolean =
        if (doNodeFilter) (n: NodeT) => ! isVisited(n) && subgraphNodes(n)
        else              (n: NodeT) => ! isVisited(n)
        
      final private[this] def filtered(
          nodes:     AnySet[NodeT],
          maxNodes:  Int,                             
          isVisited: (NodeT) => Boolean,
          reverse:   Boolean): AnySet[NodeT] = {
        val filtered = new FilteredSet(nodes, chooseFilter(isVisited))
        if(doNodeSort) sortedNodes(filtered, maxNodes, reverse)
        else filtered
      }
        
      @inline final private[this] def filteredEdges(edges: AnySet[EdgeT])
          : FilterMonadic[EdgeT,AnySet[EdgeT]] =
        if (doEdgeFilter) edges withFilter subgraphEdges
        else edges
        
      final private[this] def filteredSuccessors[U](
          node:        NodeT,
          isVisited:   (NodeT) => Boolean,
          edgeVisitor: EdgeT => U,
          reverse:     Boolean): Traversable[NodeT] = {
  
        if (withEdgeFiltering(edgeVisitor))
          filtered(node, isVisited, filteredEdges(node.outgoing), edgeVisitor, reverse)
        else {
          val succ = node.diSuccessors
          filtered(succ, succ.size, isVisited, reverse)
        }
      }
        
      final private[this] def filteredPredecessors[U](
          node:        NodeT,
          isVisited:   (NodeT) => Boolean,
          edgeVisitor: EdgeT => U,
          reverse:     Boolean): Traversable[NodeT] = {
        
        if (withEdgeFiltering(edgeVisitor))
          filtered(node, isVisited, filteredEdges(node.incoming), edgeVisitor, reverse)
        else
          filtered(node.diPredecessors, - estimatedNrOfNodes(node), isVisited, reverse)
      }
        
      final private[this] def filteredNeighbors[U](
          node:        NodeT,
          isVisited:   (NodeT) => Boolean,
          edgeVisitor: EdgeT => U,
          reverse:     Boolean): Traversable[NodeT] = {
            
        if (withEdgeFiltering(edgeVisitor))
          filtered(node, isVisited, filteredEdges(node.edges), edgeVisitor, reverse)
        else
          filtered(node.neighbors, - estimatedNrOfNodes(node), isVisited, reverse)
      }
  
      final protected[collection] def shortestPathTo[T:Numeric, U](
          potentialSuccessor: NodeT,
          weight            : EdgeT => T): Option[Path] = {
        withHandle() { implicit visitedHandle => 
          val num = implicitly[Numeric[T]] 
          import num._
          @inline def visited(n: NodeT) = n.visited
  
          type NodeWeight = (NodeT,T)
          val weightOrdering = Edge.weightOrdering(weight)
          val dest      = MMap[NodeT,T](root -> zero)
          val mapToPred = MMap[NodeT,NodeT]()
          // not implicit due to issues #4405 and #4407
          object ordNodeWeight extends Ordering[NodeWeight] {
            def compare(x: NodeWeight,
                        y: NodeWeight) = num.compare(y._2, x._2)
          }
          val qNodes = new PriorityQueue[NodeWeight]()(ordNodeWeight) += ((root -> zero))
  
          def sortedAdjacentNodes(node: NodeT): PriorityQueue[NodeWeight] =
            filteredSuccessors(node, visited, edgeVisitor, false).foldLeft(
                new PriorityQueue[NodeWeight]()(ordNodeWeight))(
                (q,n) => q += ((n, dest(node) +
                                   weight(node.outgoingTo(n).withFilter(subgraphEdges(_)).
                                          min(weightOrdering))
                               ))
                )
          def relax(pred: NodeT, succ: NodeT) {
            val cost = dest(pred) + weight(pred.outgoingTo(succ).withFilter(subgraphEdges(_)).
                                           min(weightOrdering))
            if(! dest.isDefinedAt(succ) || cost < dest(succ)) {
              dest      += (succ->cost)
              mapToPred += (succ->pred)
            }
          }
          var nodeCnt = 0
          @tailrec def rec(pq: PriorityQueue[NodeWeight]) {
            if(pq.nonEmpty && (pq.head._1 ne potentialSuccessor)) { 
              val nodeWeight = pq.dequeue
              val node = nodeWeight._1
              if (! node.visited) {
                val ordNodes = sortedAdjacentNodes(node)
                pq ++= ordNodes
                @tailrec def loop(pq2: PriorityQueue[NodeWeight]) {
                  if (pq2.nonEmpty) {
                    relax(node, pq2.dequeue._1)
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
                        def queueIterator = qNodes.toIterator
                        def costsIterator = dest.toIterator
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
                        
      final protected[collection] def bfs[U]: Option[NodeT] = {
        withHandle() { implicit visitedHandle => 
          val untilDepth =
            if (parameters.maxDepth > 0) parameters.maxDepth
            else java.lang.Integer.MAX_VALUE
          var depth = 0
          var nodeCnt = 0
          val q = Queue[(NodeT, Int)](root -> depth)
          @inline def visited(n: NodeT) = n.visited  
          def visit(n: NodeT): Unit = {
            n.visited = true
            if (doNodeVisitor)
              if (isDefined(nodeVisitor)) nodeVisitor(n)
              else {
                nodeCnt += 1
                extNodeVisitor(n, nodeCnt, depth,
                  new BfsInformer[NodeT] {
                    def queueIterator = q.toIterator 
                  }
                )
              }
          }
          visit(root)
          while (q.nonEmpty) { 
            val (prevNode, prevDepth) = q.dequeue
            if (prevDepth < untilDepth) {
              depth = prevDepth + 1
              for (node <- filteredNodes(prevNode, visited, edgeVisitor, false)) { 
                visit(node)
                if (stopAt(node)) return Some(node)
                q enqueue (node -> depth)  
              }
            }
          }
          None
        }
      }
  
      @inline final protected[collection]
      def dfsNode[U](nodeUpVisitor: (NodeT) => U): Option[NodeT] =
        dfs(nodeUpVisitor)._1
  
      final protected[collection] def dfs[U](nodeUpVisitor: (NodeT) => U)
          : (Option[NodeT], Stack[(NodeT, Int)]) =
      withHandle() { implicit visitedHandle => 
        val untilDepth: Int =
          if (parameters.maxDepth > 0) parameters.maxDepth
          else java.lang.Integer.MAX_VALUE
        val doNodeUpVisitor = isDefined(nodeUpVisitor)
  
        @inline def isVisited(n: NodeT): Boolean = n.visited
        val stack: Stack[(NodeT, Int)] = Stack((root, 0))
        val path:  Stack[(NodeT, Int)] = Stack()
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
            if (doNodeVisitor)
              if (isDefined(nodeVisitor)) nodeVisitor(current)
              else {
                nodeCnt += 1
                extNodeVisitor(current, nodeCnt, depth,
                  new DfsInformer[NodeT] {
                    def stackIterator = stack.toIterator 
                    def pathIterator  = path .toIterator
                  }
                )
              }
            if (stopAt(current) && (current ne root)) {
              res = Some(current)
            } else {
              if (depth < untilDepth)
                for (n <- filteredNodes(current, isVisited, edgeVisitor, true)
                          withFilter (! isVisited(_))) {
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
  
      /** Tail-recursive white-gray-black DFS implementation for cycle detection.
       * 
       * @param root start node for the search
       * @param stop node predicate marking an end condition for the search
       */
      final protected[collection]
      def dfsWGB[U](globalState: Array[Handle] = Array.empty[Handle])
          : (Option[NodeT], Stack[CycleStackElem]) = {
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
                if (doNodeVisitor)
                  if (isDefined(nodeVisitor)) nodeVisitor(current)
                  else {
                    nodeCnt += 1
                    extNodeVisitor(current, nodeCnt, 0,
                        new WgbInformer[NodeT, EdgeT] {
                          def stackIterator = stack.toIterator 
                          def pathIterator  = path .toIterator 
                      }
                    )
                  }
                if (stopAt(current) && (current ne root))
                  res = Some(current)
                else {
                  var pushed = false
                  for (n <- filteredNodes(current, isBlack(_), edgeVisitor, true)
                            withFilter (! isBlack(_))) { 
                    if (isGray(n)) {
                      if (exclude.fold(ifEmpty = true)(_ ne n))
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
    }

    protected[collection] object Runner {
      @inline final def apply[U](stopAt:  (NodeT) => Boolean,
                                 visitor: A => U): Runner[U] = new Runner[U](stopAt, visitor)
    }
  }
}