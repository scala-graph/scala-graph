package scalax.collection

import language.{higherKinds, implicitConversions}
import scala.annotation.{switch, tailrec}

import collection.mutable.{ArrayBuffer, ListBuffer, Queue, PriorityQueue, ArrayStack => Stack,
                           Set => MutableSet, Map => MutableMap}
import GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, GraphParamOut,
                    NodeIn, NodeOut, EdgeIn, EdgeOut}
import GraphEdge.{EdgeLike}
import mutable.{ArraySet, ExtBitSet}
import scalax.collection.mutable.ExtBitSet
import scalax.collection.mutable.ExtBitSet

trait GraphTraversalImpl[N, E[X] <: EdgeLikeIn[X]]
  extends GraphTraversal[N,E]
  with State[N,E]
{ selfGraph =>

  import GraphTraversalImpl._
  import GraphTraversal.VisitorReturn._
  import GraphTraversal._
  import State._
  
  override def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                         edgeFilter : (EdgeT) => Boolean       = anyEdge,
                         maxDepth   :  Int                     = 0,
                         nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                         edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                         ordering   : ElemOrdering             = noOrdering): Option[Cycle] =
    if (order == 0) None
    else {
      val path = CycleBuffer(nodeFilter, edgeFilter, isDirected)
      val traversal = new Traversal(Successors, nodeFilter, edgeFilter,
                                                nodeVisitor, edgeVisitor, ordering)
      withHandles(2) { handles => 
        implicit val visitedHandle = handles(0) 
        for (node <- nodes if ! node.visited) {
          val res = traversal.depthFirstSearchWGB(
                      node,
                      onPopFound = (n, conn) => path. +=: (n, conn),
                      globalState = handles) 
          path.close
          if (res.isDefined)
            return Some(path)
        }
      }
      None
    }
  type NodeT <: InnerNodeTraversalImpl
  trait InnerNodeTraversalImpl extends super.InnerNodeLike with InnerNodeState
  { this: NodeT =>
    override def findSuccessor(pred: (NodeT) => Boolean,
                               nodeFilter : (NodeT) => Boolean       = anyNode,
                               edgeFilter : (EdgeT) => Boolean       = anyEdge,
                               nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                               edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                               ordering   : ElemOrdering             = noOrdering) =
    {
      new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor, ordering).
          depthFirstSearch(this, pred, 0)
    }
    override def findPredecessor(pred: (NodeT) => Boolean,
                                 nodeFilter : (NodeT) => Boolean       = anyNode,
                                 edgeFilter : (EdgeT) => Boolean       = anyEdge,
                                 nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                                 edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                                 ordering   : ElemOrdering             = noOrdering) =
    {
      new Traversal(Predecessors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor, ordering).
          depthFirstSearch(this, pred = pred)
    }
    override def findConnected(pred: (NodeT) => Boolean,
                               nodeFilter : (NodeT) => Boolean       = anyNode,
                               edgeFilter : (EdgeT) => Boolean       = anyEdge,
                               nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                               edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                               ordering   : ElemOrdering             = noOrdering) =
    {
      new Traversal(AnyConnected, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor, ordering).
          depthFirstSearch(this, pred)
    }
    override def pathUntil(pred: (NodeT) => Boolean,
                           nodeFilter : (NodeT) => Boolean       = anyNode,
                           edgeFilter : (EdgeT) => Boolean       = anyEdge,
                           nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                           edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                           ordering   : ElemOrdering             = noOrdering): Option[Path] = {
      val path = PathBuffer(nodeFilter, edgeFilter)
      if (new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor, ordering).
          depthFirstSearch(this,
                           pred,
                           onPopFound = (n: NodeT) => {
                             if (path.isEmpty) path. +=: (n)
                             else              path. +=: (n, path.firstEdge _) 
                           }).isDefined)
        Some(path.+=:(this, path.firstEdge _))
      else
        None
    }
    override def shortestPathTo(to: NodeT,
                                nodeFilter : (NodeT) => Boolean       = anyNode,
                                edgeFilter : (EdgeT) => Boolean       = anyEdge,
                                nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                                edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                                ordering   : ElemOrdering             = noOrdering): Option[Path] =
    {
      withHandle() { implicit visitedHandle => 
        @inline def visited(n: NodeT) = n.visited
  
        type NodeWeight    = (NodeT,Long)
        val dest      = MutableMap[NodeT,Long](this -> 0L)
        val mapToPred = MutableMap[NodeT,NodeT]()
        val traversal = new Traversal(Successors, nodeFilter, edgeFilter,
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
                      y: NodeWeight) = y._2.compare(x._2)
        }
        val qNodes = new PriorityQueue[NodeWeight]()(ordNodeWeight) += ((this->0L))
  
        def sortedAdjacentsNodes(node: NodeT): Option[PriorityQueue[NodeWeight]] = 
          traversal.filteredDiSuccessors(node, visited, false) match {
            case adj if adj.nonEmpty =>
              Some(adj.
                   foldLeft(new PriorityQueue[NodeWeight]()(ordNodeWeight))(
                     (q,n) => q += ((n, dest(node) +
                                        node.outgoingTo(n).filter(edgeFilter(_)).
                                        min(Edge.WeightOrdering).weight))))
            case _ => None
          }
        def relax(pred: NodeT, succ: NodeT) {
          val cost = dest(pred) + pred.outgoingTo(succ).filter(edgeFilter(_)).
                                  min(Edge.WeightOrdering).weight
          if(!dest.isDefinedAt(succ) || cost < dest(succ)) {
            dest      += (succ->cost)
            mapToPred += (succ->pred)
          }
        }
        var nodeCnt = 0
        var canceled = false
        @tailrec
        def rec(pq: PriorityQueue[NodeWeight]) {
          if(pq.nonEmpty && (pq.head._1 ne to)) { 
            val nodeWeight = pq.dequeue
            val node = nodeWeight._1
            if (!node.visited) {
              sortedAdjacentsNodes(node) match {
                case Some(ordNodes) =>
                  if (ordNodes.nonEmpty) pq ++= (ordNodes)
                  @tailrec
                  def loop(pq2: PriorityQueue[NodeWeight]) {
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
                  new DijkstraInformer[NodeT] {
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
          val path = PathBuffer(nodeFilter, edgeFilter).+=:(to)
          if(map.isDefinedAt(to)) {
            @tailrec
            def loop(k: NodeT) {
              path.+=:(k, path.minWeightEdge _)
              if(map.isDefinedAt(k))
                loop(map.get(k).get) 
            }
            loop(map.get(to).get);
            Some(path)
          } else if(this eq to) Some(path) else None
        }
        if (canceled) None
        else traverseMapNodes(mapToPred)
      }
    }
    override def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                           edgeFilter : (EdgeT) => Boolean       = anyEdge,
                           maxDepth   :  Int                     = 0,
                           nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                           edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                           ordering   : ElemOrdering             = noOrdering) =
    {
      val path = CycleBuffer(nodeFilter, edgeFilter, isDirected)
      if (new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor, ordering).
          depthFirstSearchWGB (this,
                               onPopFound = (n, conn) => path. +=: (n, conn)
          ).isDefined) {
        path.close
        Some(path)
      } else None
    }
    final override
    def traverse (direction  : Direction          = Successors,
                  nodeFilter : (NodeT) => Boolean = anyNode,
                  edgeFilter : (EdgeT) => Boolean = anyEdge,
                  breadthFirst:Boolean            = true,
                  maxDepth   :  Int               = 0,
                  ordering   : ElemOrdering       = noOrdering)
                 (nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                  edgeVisitor: (EdgeT) => Unit           = noEdgeAction)
    {
      new Traversal(direction, nodeFilter,  edgeFilter, nodeVisitor, edgeVisitor, ordering).
        apply(this, noNode, breadthFirst, maxDepth)
    }
  }

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
    override def depthFirstSearch(root         : NodeT,
                                  pred         : (NodeT) => Boolean = noNode,
                                  maxDepth     : Int                = 0,
                                  nodeUpVisitor: (NodeT) => Unit    = noNodeUpAction, 
                                  onPopFound   : (NodeT) => Unit    = noAction): Option[NodeT] =
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
      if (res.isDefined) {
        if (onPopFound ne noAction) {
          var node = path.pop._1
          do { onPopFound(node)
               node = path.pop._1
          } while(node ne root)
        }
      }
      res
    }
    /**
     * Tail-recursive DFS implementation for cycle detection based on the idea of
     * the white-gray-black algorithm.
     * 
     * @param root start node for the search
     * @param predicate node predicate marking an end condition for the search
     * @param onPopFound action to be carried out for every node beginning at the
     *        node found by the search and ending with `root`; this parameter is
     *        primarily used to build a path after a successful search. 
     */
    protected[collection]
    def depthFirstSearchWGB(root      :  NodeT,
                            predicate : (NodeT) => Boolean  = noNode,
                            onPopFound: (NodeT, Iterable[EdgeT]) => Unit,
                            globalState: Array[Handle]= Array.empty[Handle])
        : Option[NodeT] =
    {
      withHandles(2, globalState) { handles =>
        implicit val visitedHandle = handles(0)
        val blackHandle = handles(1)
        
        // (node, predecessor, exclude, 0 to 2 multi edges) with last two for undirected
        val stack: Stack[(NodeT, NodeT, Boolean, Iterable[EdgeT])] =
            Stack((root, root, false, Nil))
        // (node, connecting with prev)
        val path = Stack.empty[(NodeT, Iterable[EdgeT])]
        val isDiGraph = selfGraph.isDirected
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
        /* pushed allows to track the path.
         * prev   serves the special handling of undirected edges. */
        @tailrec
        def loop(pushed: Boolean) {
          if (res.isEmpty)
            if (stack.isEmpty)
              path foreach (t => setBlack(t._1))
            else {
              val popped = stack.pop
              val exclude: Option[NodeT] =
                if (pushed)
                  if (popped._3) Some(popped._2) else None
                else {
                  while ( path.nonEmpty &&
                         (path.head._1 ne root) &&
                         (path.head._1 ne popped._2)) {
                    val p = path.pop._1
                    if (! isBlack(p))
                      onNodeUp(p) 
                  }
                  Some(path.head._1)
                }
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
  
        if (res.isDefined) {
          if (onPopFound ne null) {
            val resNode = res.get
            if (isDiGraph) {
              onPopFound(resNode, Nil)
              var continue = true
              while(continue && path.nonEmpty) {
                val popped = path.pop 
                val n = popped._1
                onPopFound(n, popped._2)
                if (n eq resNode) continue = false
              }
            } else {
              val rev = path.result // undocumented fast reverse
              while (rev.head._1 ne resNode) rev.pop
              rev.drain { popped => 
                val n = popped._1
                onPopFound(n, popped._2)
              }
              onPopFound(resNode, Nil)
            }
          }
        }
        res
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
  /**
   * Serves as a buffer to be populated by graph algorithms yielding a `Path`.
   * The provided functionality is meant to help graph algorithm
   * developers by providing abstractions over ListBuffer.  
   * 
   * @author Peter Empen
   */
  class PathBuffer(val nodeFilter : (NodeT) => Boolean = anyNode,
                   val edgeFilter : (EdgeT) => Boolean = anyEdge)
    extends Path
  {
    val buf = ListBuffer[GraphParamOut[N,E]]()
    def iterator = buf.iterator
    def startNode = buf.head.asInstanceOf[NodeT]
    def endNode   = buf.last.asInstanceOf[NodeT]
    /** Prepends a node to this buffer. The caller is responsible for correct order. */
    @inline final def +=: (node: NodeT) = { buf.+=:(node); this }
    /** Prepends an edge to this buffer. The caller is responsible for correct order. */
    @inline final def +=: (edge: EdgeT) = { buf.+=:(edge); this }
    /** Prepends a (node, edge) pair to this buffer. */
    def +=: (node: NodeT, edge: EdgeT): this.type = {
      buf.+=:(edge); buf.+=:(node); this
    }
    /**
     * The first edge found having its tail at `from` and its head at `to`
     * and satisfying `navigation.edgeFilter`.
     * The caller must guarantee that there is at least one connecting edge
     * between `from` and `to` with `navigation.edgeFilter`.
     * This simplest select method can be passed to `+=: (node, selectEdge)`
     * whenever any of possibly several edges between the two nodes
     * satisfies the needs of the algorithm.
     */
    def firstEdge (from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        from.outgoingTo(to).filter(edgeFilter).head
      else
        from.findOutgoingTo(to).get

    /**
     * The edge with minimal weight having its tail at `from` and its head at `to`
     * and satisfying `navigation.edgeFilter`.
     * The caller must guarantee that there is at least one connecting edge
     * between `from` and `to` with `navigation.edgeFilter`.
     * This select method can be passed to `+=: (node, selectEdge)`
     * whenever the edge with the minimal weight between the two nodes
     * should be selected.
     */
    def minWeightEdge (from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        from outgoingTo to filter edgeFilter min(Edge.WeightOrdering)
      else
        from outgoingTo to min(Edge.WeightOrdering)
    /**
     * Prepends a pair of (node, edge) to this buffer. The edge is selected
     * calling `selectEdge` with the arguments `node` and `startNode` of
     * the current buffer. 
     */
    def +=: (node      : NodeT,
             selectEdge: (NodeT, NodeT) => EdgeT): this.type =
      this. +=: (node, selectEdge(node, startNode))

    def +=: (node      : NodeT,
             selectEdge: (NodeT, NodeT, (EdgeT) => Boolean) => EdgeT): this.type =
      this. +=: (node, selectEdge(node, startNode, edgeFilter))

    def isValid = {
      var mustBeNode = false
      var size = 0
      assert(
        iterator forall {elm =>
          mustBeNode = ! mustBeNode
          size += 1
          elm match { case _:Node => mustBeNode
                      case _:Edge => ! mustBeNode
                      case _      => false}} )
      size >= 1 && size % 2 == 1
    } 
    override def canEqual(that: Any) = that.isInstanceOf[GraphTraversalImpl[N,E]#PathBuffer]
    override def equals(other: Any) = other match {
      case that: GraphTraversalImpl[N,E]#PathBuffer => 
        (this eq that) ||
        (that canEqual this) &&
        (that.buf sameElements this.buf)
      case _ => false
    }
    override def hashCode = buf.##  
  }
  object PathBuffer
  {
    def apply(nodeFilter : (NodeT) => Boolean = anyNode,
              edgeFilter : (EdgeT) => Boolean = anyEdge) =
      new PathBuffer(nodeFilter, edgeFilter)
    /**
     * Enables to treat an instance of PathBuffer as if it was a ListBuffer. 
     * @param pathBuf The PathBuffer to convert.
     * @return The buffer contained in `pathBuf`
     */
    @inline final
    implicit def toBuffer(pathBuf: PathBuffer): ListBuffer[GraphParamOut[N,E]] = pathBuf.buf 
  }
  class CycleBuffer(nodeFilter : (NodeT) => Boolean = anyNode,
                    edgeFilter : (EdgeT) => Boolean = anyEdge,
                    isDiGraph  : Boolean)
    extends PathBuffer(nodeFilter, edgeFilter)
    with    Cycle
  {
    import mutable.EqSet, mutable.EqSet.EqSetMethods
    final protected val multi = if (isDiGraph) null else EqSet[EdgeT](graphSize / 2) 
    
    protected[collection] def close: this.type = {
      if (! isDiGraph) multi.clear
      this
    }
        
    final def +=: (n: NodeT, conn: Iterable[EdgeT]): this.type = {
      if (buf.isEmpty) buf. +=: (n)
      else if (isDiGraph) +=: (n, firstEdge _)
      else {
        val edge = (conn.size: @switch) match {
          case 0 => n.edges.find (e => ! multi.contains(e) &&
                                       edgeFilter(e) &&
                                       e.hasTarget((x: NodeT) => x eq startNode)).get          
          case 1 => conn.head
          case _ => conn.find (e => ! multi.contains(e) &&
                                    edgeFilter(e) &&
                                    e.hasSource((x: NodeT) => x eq n)).get
        }
        +=: (n, edge)
        multi += edge
      }
      this
    }

    final def sameAs(that: GraphTraversal[N,E]#Cycle) =
      this == that || ( that match {
        case that: GraphTraversalImpl[N,E]#CycleBuffer =>
          this.size == that.size && {
            val idx = this.buf.indexOf(that.head)
            if (idx > 0) {
              val thisDoubled = this.buf.toList ++ (this.buf.tail)
              (thisDoubled startsWith (that.buf        , idx)) ||
              (thisDoubled startsWith (that.buf.reverse, idx))
            }
            else false
          }
        case _ => false
      })
    def dependentCycles(nodeFilter : (NodeT) => Boolean       = anyNode,
                        edgeFilter : (EdgeT) => Boolean       = anyEdge,
                        maxDepth   :  Int                     = 0,
                        nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                        edgeVisitor: (EdgeT) => Unit          = noEdgeAction) = {
      def isDependent(a: Cycle, b: Cycle) =
        (a.nodes.toSet & b.nodes.toSet).nonEmpty
      val excluded = List.empty[EdgeT]
      for(e <- edges; exclude = excluded :+ e;
          n <- (e.edge filter (_ ne e.edge._1));
          opt = n findCycle(nodeFilter,
                                edgeFilter(_) && ! excluded.exists(_ eq e),
                                maxDepth,
                                nodeVisitor,
                                edgeVisitor) filter (c => isDependent(c, this)) 
            if opt.isDefined)
        yield opt.get  
    }
  }
  object CycleBuffer
  {
    def apply(nodeFilter : (NodeT) => Boolean = anyNode,
              edgeFilter : (EdgeT) => Boolean = anyEdge,
              idDirected : Boolean) =
      new CycleBuffer(nodeFilter, edgeFilter, isDirected)
    /**
     * Enables to treat an instance of CycleBuffer as if it was a ListBuffer. 
     * @param cycleBuf The CycleBuffer to convert.
     * @return The buffer contained in `cycleBuf`
     */
    @inline final
    implicit def toBuffer(cycleBuf: CycleBuffer): ListBuffer[GraphParamOut[N,E]] = cycleBuf.buf 
  }
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
  trait DijkstraInformer[N] extends NodeInformer {
    import DijkstraInformer._
    def queueIterator: DijkstraQueue[N]
    def costsIterator: DijkstraCosts[N]
  }
  object DijkstraInformer {
    type DijkstraQueue[N] = Iterator[(N, Long)]
    type DijkstraCosts[N] = Iterator[(N, Long)]
    def unapply[N](inf: DijkstraInformer[N])
        : Option[(DijkstraQueue[N], DijkstraCosts[N])] =
      Some(inf.queueIterator, inf.costsIterator)
  }
} 