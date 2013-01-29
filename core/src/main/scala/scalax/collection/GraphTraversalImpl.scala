package scalax.collection

import annotation.tailrec
import collection.mutable.{ArrayBuffer, ListBuffer, Queue, PriorityQueue, Stack,
                           Set => MutableSet, Map => MutableMap}

import GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, GraphParamOut,
                    NodeIn, NodeOut, EdgeIn, EdgeOut}
import GraphEdge.{EdgeLike}
import mutable.ArraySet

trait GraphTraversalImpl[N, E[X] <: EdgeLikeIn[X]] extends GraphTraversal[N,E]
{
  import GraphTraversal.VisitorReturn._
  import GraphTraversal._
  /*
  override def components(nodeFilter : (NodeT) => Boolean       = anyNode,
                          edgeFilter : (EdgeT) => Boolean       = anyEdge,
                          nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                          edgeVisitor: (EdgeT) => Unit          = noEdgeAction) =
    if (order == 0) List.empty[Set[NodeT]]
    else {
      val all = nodes filter (nodeFilter(_))
      val collected = MutableSet.empty[NodeT]
      val traversal = new Traversal(
          AnyConnected, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor)
      var next = nodes.head 
      while (! (collected contains next))
        traversal.depthFirstSearch(next).found.isDefined)
      ...
    }
  */
  override def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                         edgeFilter : (EdgeT) => Boolean       = anyEdge,
                         maxDepth   :  Int                     = 0,
                         nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                         edgeVisitor: (EdgeT) => Unit          = noEdgeAction,
                         ordering   : ElemOrdering             = noOrdering): Option[Cycle] =
    if (order == 0) None
    else {
      val path = CycleBuffer(nodeFilter, edgeFilter)
      val traversal = new Traversal(Successors, nodeFilter, edgeFilter,
                                                nodeVisitor, edgeVisitor, ordering)
      State.withHandles(2) { handles => 
        implicit val visitedHandle = handles(0) 
        for (node <- nodes if ! node.visited) {
          val res = traversal.depthFirstSearchWGB(
                      node,
                      onPopFound = (n: NodeT) => {
                        if (path.isEmpty) path. +=: (n)
                        else              path. +=: (n, path.firstEdge _)},
                      globalState = handles) 
          if (res.isDefined)
            return Some(path)
        }
      }
      None
    }
  type NodeT <: InnerNodeTraversalImpl
  trait InnerNodeTraversalImpl extends super.InnerNodeLike
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
      State.withHandle() { implicit visitedHandle => 
        @inline def visited(n: NodeT) = n visited
  
        type NodeWeight    = Tuple2[NodeT,Long]
        lazy val dest      = MutableMap[NodeT,Long](this->0L)
        lazy val mapToPred = MutableMap[NodeT,NodeT]()
        lazy val traversal = new Traversal(Successors, nodeFilter, edgeFilter,
                                           nodeVisitor, edgeVisitor, ordering) 
        lazy val doNodeVisitor = isCustomNodeVisitor(nodeVisitor)
        // not implicit - see ticket #4405 and #4407
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
                     (q,n) => q += ((n, node.outgoingTo(n).filter(edgeFilter(_)).
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
        var canceled = false
        @tailrec
        def rec(pq: PriorityQueue[NodeWeight]) {
          if(pq.nonEmpty && (pq.head._1 ne to)) { 
            val nodeWeight = pq.dequeue
            val node = nodeWeight._1 
            sortedAdjacentsNodes(node) match {
              case Some(ordNodes) =>
                if (ordNodes.nonEmpty) pq ++=(ordNodes)
                @tailrec
                def loop(pq2: PriorityQueue[NodeWeight]) {
                  if(pq2.nonEmpty) {
                    relax(node, pq2.dequeue._1)
                    loop(pq2)
                  }
                }
                loop(ordNodes)
              case None =>
            }
            node.visited = true
            if (doNodeVisitor) 
              if (nodeVisitor(node) == Cancel) {
                canceled = true
                return
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
      val path = CycleBuffer(nodeFilter, edgeFilter)
      if (new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor, ordering).
          depthFirstSearchWGB (this,
                               onPopFound = (n: NodeT) => {
                                 if (path.isEmpty) path. +=: (n)
                                 else              path. +=: (n, path.firstEdge _) 
                               }).isDefined)
        Some(path)
      else
        None
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
                                
    @transient private var flags = State.emptyFlags
    @inline final protected[collection]
    def bit[T](implicit handle: State.Handle): Boolean =
      State.bit(flags, handle) 
    /** Whether this node is marked as visited with respect to `handle`. */
    @inline final protected[collection]
    def visited(implicit handle: State.Handle): Boolean =
      bit(handle) 
    @inline final protected[collection]
    def bit_=[T](visited: Boolean)(implicit handle : State.Handle) {
      synchronized {
        flags = State.bit_=(flags, visited, handle)
      }
    }
    /** Sets this node to `visited` with respect to to `handle`. */
    @inline final protected[collection]
    def visited_= (visited: Boolean)(implicit handle: State.Handle) {
      bit_= (visited)(handle)
    }

    /** Resets stateful data with respect to dirty bits.
     *  @param dirtyFlags bits to be set to 0
     */
    @inline final protected[collection] def clear(dirtyFlags: StateFlags) {
      synchronized {
        flags &= ~dirtyFlags
      }
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
    State.withHandle() { implicit visitedHandle => 
      val stack: Stack[(NodeT, Int)] = Stack((root, 0))
      val path:  Stack[(NodeT, Int)] = Stack()
      val untilDepth: Int = if (maxDepth > 0) maxDepth else java.lang.Integer.MAX_VALUE
      @inline def isVisited(n: NodeT): Boolean = n visited  
      val doNodeUpVisitor = isCustomNodeUpVisitor(nodeUpVisitor)
      var res: Option[NodeT] = None
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
          if (doNodeVisitor && nodeVisitor(current) == Cancel)
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
                            onPopFound: (NodeT) => Unit     = noAction,
                            globalState: Array[State.Handle]= Array.empty[State.Handle])
        : Option[NodeT] =
    {
      State.withHandles(2, globalState) { handles =>
        implicit val visitedHandle = handles(0)
        val blackHandle = handles(1)
        val stack = Stack((root, root)) // (node, predecessor)
        val path = Stack.empty[NodeT]
        def isWhite (node: NodeT) = nonVisited(node)
        def isGray  (node: NodeT) = isVisited(node) && ! (node bit(blackHandle))
        def isBlack (node: NodeT) = node bit(blackHandle)
        def setGray (node: NodeT) { node.visited = true }
        def setBlack(node: NodeT) { node.bit_=(true)(blackHandle) } 
  
        def onNodeDown(node: NodeT) { setGray (node) } 
        def onNodeUp  (node: NodeT) { setBlack(node) }
  
        def isVisited (node: NodeT) = node visited
        def nonVisited(node: NodeT) = ! isVisited(node)
        var res: Option[NodeT] = None
        /* pushed allows to track the path.
         * prev   serves the special handling of undirected edges. */
        @tailrec
        def loop(pushed: Boolean, prev: Option[NodeT]) {
          if (res.isEmpty)
            if (stack.isEmpty)
              path foreach (setBlack(_))
            else {
              var newPrev = prev
              val exclude = if (pushed) prev else {
                while ( path.nonEmpty &&
                       (path.head ne root) &&
                       (path.head ne stack.head._2)) {
                  val p = path.pop
                  if (! isBlack(p))
                    onNodeUp(p) 
                }
                Some(path.head)
              }
              val current = stack.pop._1
              path.push(current)
              if (nonVisited(current)) onNodeDown(current)
              if (doNodeVisitor && nodeVisitor(current) == Cancel) return
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
                    stack.push((n, current))
                    pushed = true
                    newPrev = if (current.outgoingTo(n) exists (!_.directed)) Some(current)
                              else None
                  }
                }
                loop(pushed, newPrev)
              }
            }
        }
        loop(true, None)
  
        if (res.isDefined) {
          if (onPopFound ne noAction) {
            val resNode = res.get
            onPopFound(resNode)
            var continue = true
            while(continue && path.nonEmpty) {
              val n = path.pop
              onPopFound(n)
              if (n eq resNode) continue = false
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
      State.withHandle() { implicit visitedHandle => 
        val untilDepth = if (maxDepth > 0) maxDepth else java.lang.Integer.MAX_VALUE
        var depth = 0
        val doNodeVisitor = isCustomNodeVisitor(nodeVisitor)
        @inline def visited(n: NodeT) = n visited  
        @inline def visitAndCanceled(n: NodeT) = {
          n.visited = true
          doNodeVisitor && nodeVisitor(n) == Cancel
        }
        if (visitAndCanceled(root)) return None
        val q = Queue[(NodeT, Int)](root -> depth)
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
        from outgoingTo to filter edgeFilter head
      else
        from findOutgoingTo to get
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
     * the correct buffer. 
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
    override def hashCode = buf ##  
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
                    edgeFilter : (EdgeT) => Boolean = anyEdge)
    extends PathBuffer(nodeFilter, edgeFilter)
    with    Cycle
  {
    def sameAs(that: GraphTraversal[N,E]#Cycle) =
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
        (a.nodes.toSet & b.nodes.toSet) nonEmpty
      val excluded = List.empty[EdgeT]
      for(e <- edges; val exclude = excluded :+ e;
          n <- (e.edge filter (_ ne e.edge._1));
          val opt = n findCycle(nodeFilter,
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
              edgeFilter : (EdgeT) => Boolean = anyEdge) =
      new CycleBuffer(nodeFilter, edgeFilter)
    /**
     * Enables to treat an instance of CycleBuffer as if it was a ListBuffer. 
     * @param cycleBuf The CycleBuffer to convert.
     * @return The buffer contained in `cycleBuf`
     */
    @inline final
    implicit def toBuffer(cycleBuf: CycleBuffer): ListBuffer[GraphParamOut[N,E]] = cycleBuf.buf 
  }

  /** Boolean state information stored by `Graph` elements (nodes). State flags are 
   *  represented by the bits of an `Int`. These flags are mainly used to store
   *  whether an element counts as visited with respect to a given traversal. */
  type StateFlags = Int
  /**
   * Decoupled implementation of state for `Graph` elements.
   * State instances may carry data bound to multiple processes such as traversals.
   * To distinguish between processes they communicate with state instances by `Handle`s.
   * Parallel processes are not yet supported.
   * Currently state is just used to store whether an element counts as visited.
   * For this purpose the bits of an Int are used as flags.
   */
  protected object State extends Serializable {
    /** Initializes unset flags. */
    @transient val emptyFlags: StateFlags = 0
    /** Whether `state` is visited with respect to `handle`. */
    @inline def bit  [T](flags:  StateFlags,
                         handle: Handle): Boolean =
      (flags & handle) != 0 
    /** Sets `state` to `visited` with respect to `handle`. */
    @inline def bit_=[T](flags:   StateFlags,
                         visited: Boolean,
                         handle:  Handle): StateFlags =
      if (visited) flags |  handle
      else         flags & ~handle

    /** state accessor with respect to a given process. */
    type Handle = Int
    @transient private var lastHandle: Handle = 0
    @transient private var flagsInUse: Handle = 0
    @transient private var dirtyFlags: Handle = 0
    @inline private def    existsFreeFlag: Boolean = ! notExistsFreeFlag      
    @inline private def notExistsFreeFlag: Boolean = ~flagsInUse == 0

    /** Avoid calling this directly, prefer `withHandle` instead. */
    protected[collection] def nextHandle = synchronized {
      while (notExistsFreeFlag) wait

      if ((flagsInUse | dirtyFlags) == ~0) {
        nodes foreach (_.clear(dirtyFlags))
        dirtyFlags = 0
      }
      val freeFlags = ~(flagsInUse | dirtyFlags)
      var next = lastHandle
      do {
        next = next << 1
        if (next == 0) next = 1
      } while ((next & freeFlags) == 0)
      lastHandle = next
      flagsInUse = flagsInUse | next 
      next
    }
    /** Avoid calling this directly, prefer `withHandle` instead. */
    protected[collection] def releaseHandle(handle: Handle) = synchronized {
      flagsInUse = flagsInUse & ~handle
      dirtyFlags = dirtyFlags | handle
      notify
    }
    /** Executes a code block in the context of a new or reused state handler.
     *  @return The result of the code block executed.
     */
    protected[collection]
    def withHandle[T](reuse: Option[Handle] = None)(
                      block: Handle => T          ): T = {
      val thisHandler = reuse getOrElse nextHandle
      val res = block(thisHandler)
      if (reuse isEmpty) releaseHandle(thisHandler)
      res
    }
    /** Executes a code block in the context `nr` new state handlers
     *  or alternatively in the context of the state handlers `reuse`.
     *  @return The result of the code block executed.
     */
    protected[collection]
    def withHandles[T](nr:    Int,
                       reuse: Array[Handle] = Array.empty[Handle])(
                       block: Array[Handle] => T): T = {
      val newHandlers = reuse isEmpty
      val theseHandles =
        if (newHandlers) Array.fill(nr)(nextHandle)
        else reuse
      val res = block(theseHandles)
      if (newHandlers) theseHandles foreach releaseHandle
      res
    }
  }
}
