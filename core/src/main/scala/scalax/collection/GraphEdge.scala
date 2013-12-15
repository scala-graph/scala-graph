package scalax.collection

import language.{higherKinds, postfixOps}
import scala.annotation.switch
import scala.collection.mutable.Set

import GraphPredef.{NodeOut, EdgeIn}
import edge.LBase.LEdge

/**
 * This object serves as a container for all edge-types to be used in the context of `Graph`.
 * You will usually simply import all its members along with the members of GraphParam:
 * {{{
 * import scalax.collection.GraphPredef._, scalax.collection.GraphEdge,_
 * }}}
 * @author Peter Empen
 */
object GraphEdge {
  /**
   * Template for Edges in a Graph.
   * 
   * Implementation note: Irrespective of the containing `Graph` all library-provided Edges
   * are immutable.
   * 
   * @tparam N the user type of the nodes (ends) of this edge.
   * @author Peter Empen
   * @define CalledByValidate This function is called on every edge-instantiation
   *         by `validate` that throws EdgeException if this method returns `false`.
   * @define SHORTCUT Allows to replace the edge object with it's shortcut like
   * @define ISAT In case this edge is undirected this method maps to `isAt`
   * @defBYEQ the check is made by means of `eq` 
   */
  sealed trait EdgeLike[+N] extends Iterable[N] with Eq with Serializable
  {
    /** The end nodes joined by this edge.
     * 
     * Nodes will typically be represented by Tuples. Alternatively subclasses of `Iterable`
     * implementing Product, such as List, may also be used.
     * In the latter case be aware of higher memory footprint.
     */
    def nodes: Product
    /** Iterator for the nodes (end-points) of this edge.
     */
    def iterator: Iterator[N]
    /** Sequence of the end points of this edge.
     */
    def nodeSeq: Seq[N] = iterator.toSeq
    /**
     * The first node. Same as _n(0).
     */
    @inline final def _1: N = nodes.productElement(0).asInstanceOf[N] 
    /**
     * The second node. Same as _n(1).
     */
    def _2: N =  nodes match {
      case i: Iterable[N] => i.drop(1).head
      case p: Product     => nodes.productElement(1).asInstanceOf[N] 
    }  
    /**
     * The n'th node with 0 <= n < arity.
     */
    def _n(n: Int): N = (n: @scala.annotation.switch) match {
      case 0 => _1
      case 1 => _2
      case _ => {
        if (n < 0 || n >= arity) throw new IndexOutOfBoundsException
        nodes match {
          case i: Iterable[N] => i.drop(n).head
          case p: Product     => nodes.productElement(n).asInstanceOf[N] 
        }
      }  
    }
    /**
     * Number of nodes linked by this Edge. At least two nodes are linked. In case of
     * a hook, the two nodes are identical. Hyper-edges may link more than two nodes.
     */
    final def arity = nodes match {
      case i: Iterable[N] => i.size
      case p: Product     => nodes.productArity 
    } 
    /**
     * A function to determine whether the `arity` of the passed `Product`
     * of nodes (that is the number of edge ends) is valid.
     * $CalledByValidate
     */
    protected def isValidArity(size: Int): Boolean
    /**
     * This method may be overridden to enforce additional validation at edge
     * creation time. Be careful to call `super.isValidCustom` when overriding.
     * $CalledByValidate
     */
    protected def isValidCustom = true
    protected def isValidCustomExceptionMessage = "Custom validation failed: " + toString
    /**
     * Performs basic, inevitable edge validation. Among others, ensures
     * that `nodes ne null` and no edge end `eq null`.
     * 
     * This validation method must be called in the constructor of any edge class
     * that directly extends or mixes in `EdgeLike`. To perform additional custom
     * validation `isValidCustom` is to be overridden.
     *  
     *  @throws EdgeException if any of the basic validations or of eventually
     *  supplied additional validations fails.
     */
    protected final def validate {
      nodes match {
        case r: AnyRef if r eq null =>
          throw new EdgeException(s"null node in: $toString")
        case _ =>
      }
      val ar = arity
      if (! (ar >= 2 && isValidArity(ar)))
        throw new EdgeException("Invalid arity: " + ar + ": " + toString)
      if (! isValidCustom)
          throw new EdgeException(isValidCustomExceptionMessage)
    }
    /** `true` it this edge is directed. */
    def directed = false
    /** Same as `directed`. */
    @inline final def isDirected = directed
    /** `true` it this edge is undirected. */
    @inline final def undirected = ! directed
    /** Same as `undirected`. */
    @inline final def isUndirected = undirected
    /** `true` if this is a hyperedge that is it may have more than two ends. */
    def isHyperEdge = true
    /** `true` if this edge has exactly two ends. */
    @inline final def nonHyperEdge = ! isHyperEdge
    /** `true` if this edge produces a self-loop.
     * In case of a non-hyperedge, a loop is given if the incident nodes are equal.
     * In case of a directed hyperedge, a loop is given if the source is equal to
     * any of the targets.
     * In case of an undirected hyperedge, a loop is given if any pair of incident
     * nodes has equal nodes.
     */
    def isLooping = if (arity == 2) _1 == _2
                    else if (directed) iterator.drop(1) exists (_ == _1)
                    else (Set() ++= iterator).size < arity
    /** Same as `! looping`. */                
    final def nonLooping = ! isLooping 
    /**
     * The weight of this edge defaulting to 1L.
     * 
     * Note that `weight` is normally not part of the edge key (hashCode) meaning that
     * edges of different weights connecting the same nodes will be treated as equal
     * and not added more than once. If you need multi-edges based on different weights 
     * you should either make use of a predefined key-weighted edge type such as `WDiEdge` 
     * or define a custom edge class that mixes in `ExtendedKey` and adds `weight` to
     * `keyAttributes`. 
     * 
     * In case of weights of a type other than `Long` you either convert values of
     * that type to `Long` prior to edge creation or define a custom edge class 
     * to include your own `val` of that type and override `def weight` to provide
     * a conversion.  
     */
    def weight: Long = 1
    /**
     * The label of this edge. If `Graph`'s edge type parameter has been inferred or set
     * to a labeled edge type all contained edges are labeled. Otherwise you should
     * assert, for instance by calling `isLabeled`, that the edge instance is labeled
     * before calling this method.
     * 
     * Note that `label` is normally not part of the edge key (hashCode) meaning that
     * edges of different labels connecting the same nodes will be treated as equal
     * and not added more than once. If you need multi-edges based on different labels 
     * you should either make use of a predefined key-labeled edge type such as `LDiEdge` 
     * or define a custom edge class that mixes in `ExtendedKey` and adds `label` to
     * `keyAttributes`. 
     * 
     * @throws UnsupportedOperationException if the edge is non-labeled.
     */
    def label: Any =
      throw new UnsupportedOperationException("Call of label for a non-labeled edge.")
    /** `true` if this edge is labeled. See also `label`. */
    def isLabeled = this.isInstanceOf[LEdge[N]]

    /** Same as `isAt`. */
    @inline final def contains[M>:N](node: M): Boolean = isAt(node)

    /** `true` if `node` is incident with this edge. */
    def isAt[M>:N](node: M): Boolean
    /** `true` if any end of this edge fulfills `pred`. */
    def isAt[M>:N](pred: N => Boolean): Boolean
    
    /** `true` if `node` is a source of this edge. $ISAT. */
    def hasSource[M>:N](node: M): Boolean
    /** `true` if any source end of this edge fulfills `pred`. */
    def hasSource[M>:N](pred: N => Boolean): Boolean

    /** `true` if `node` is a target of this edge. $ISAT. */
    def hasTarget[M>:N](node: M): Boolean
    /** `true` if any target end of this edge fulfills `pred`. */
    def hasTarget[M>:N](pred: N => Boolean): Boolean
    
    /** Applies `f` to the source end resp. to all source ends of this edge. */
    def withSources(f: N => Unit): Unit
    /** Applies `f` to the target end resp. to all target ends of this edge. */
    def withTargets(f: N => Unit): Unit

    override def canEqual(that: Any) = that.isInstanceOf[EdgeLike[_]]
    override def equals(other: Any) = other match {
      case that: EdgeLike[_] => 
        (this eq that) ||
        (that canEqual this) &&
        (this.directed == that.directed) &&
        equals(that) 
      case _ => false
    }
    /**
     * Precondition: this.directed == that.directed.
     */
    protected def equals(other: EdgeLike[_]): Boolean = baseEquals(other)
    
    override def hashCode: Int = baseHashCode
  
    final protected def thisSimpleClassName = try {
      this.getClass.getSimpleName
    } catch { // Malformed class name
      case e: java.lang.InternalError => this.getClass.getName
    }
    override def stringPrefix = "Nodes"
    protected def nodesToStringWithParenthesis = false
    protected def nodesToStringSeparator = EdgeLike.nodeSeparator
    protected def nodesToString =
      if (nodesToStringWithParenthesis)
        nodes match {
          case it: Iterable[N] => it.toString.patch(0, stringPrefix, it.stringPrefix.length) 
          case _=> stringPrefix + nodes.toString
        }
      else
        iterator mkString nodesToStringSeparator
    protected def attributesToString = ""
    protected def toStringWithParenthesis = false
    protected def brackets = EdgeLike.curlyBraces
    override def toString = {
      val attr = attributesToString
      val woParenthesis = nodesToString + ( if(attr.length > 0) attr else "" )
      if (toStringWithParenthesis)
        thisSimpleClassName + brackets.left + woParenthesis + brackets.right
      else
        woParenthesis
    }
  }
  /**
   * This trait is to be mixed in by every class implementing EdgeLike.
   */
  trait EdgeCopy[+CC[X] <: EdgeLike[_]] {
    /**
     * It is a prerequisite for edge-classes to implement this method. Otherwise
     * they cannot be passed to a `Graph`.
     * 
     * `Graph` calls this method internally to obtain a new instance of the 
     * edge passed to `Graph` with nodes of the type of the inner class `NodeT`
     * which itself contains the outer node. 
     */
    protected[collection] def copy[NN](newNodes: Product): CC[NN]
  }
  object EdgeLike {
    /**
     * Number of equaling nodes.
     * @param edgeA left-hand edge
     * @param edgeB right-hand edge; may be same as left-hand edge
     * @return Number of nodes in `edgeA` being equal to a node in `edgeB` at any position. 
     */
    final def nrEqualingNodes(edgeA: EdgeLike[_], edgeB: EdgeLike[_]): Int = {
      var nr = 0
      for (a <- edgeA.iterator; b <- edgeB.iterator)
        if (a == b) nr += 1
      nr
    }
    val nodeSeparator = "~" 
    protected case class Brackets(val left: Char, val right: Char)
    protected val curlyBraces = Brackets('{', '}')
    def unapply[N](e: EdgeLike[N]) = Some(e)
  }
  /**
   * Helper object to convert edge-factory parameter-lists to tuple-n or list.
   *  
   * @author Peter Empen
   */
  object NodeProduct {
    @inline final def apply[N](node_1: N, node_2: N)     : Product = Tuple2(node_1, node_2)
    @inline def apply[N](node_1: N, node_2: N, nodes: N*): Product = {
      (nodes.size: @scala.annotation.switch) match {
        case 1 => Tuple3(node_1, node_2, nodes(0))
        case 2 => Tuple4(node_1, node_2, nodes(0), nodes(1))
        case 3 => Tuple5(node_1, node_2, nodes(0), nodes(1), nodes(2))
        case _ => List[N](node_1, node_2) ::: List(nodes: _*) 
      }
    }
    @inline final def apply[N](nodes: Iterable[N]): Product =
      if (nodes.size == 2)
        apply(nodes.head, nodes.tail)
      else {
        val nodesTail = nodes.tail
        apply(nodes.head, nodesTail.head, nodesTail.tail.toSeq: _*)
      }
  }
  protected[collection] sealed trait Eq {
    protected def baseEquals(other: EdgeLike[_]): Boolean
    protected def baseHashCode: Int
  }
  protected[collection] trait EqHyper extends Eq {
    this: EdgeLike[_] =>

    override protected def baseEquals(other: EdgeLike[_]) =
      this.arity == other.arity &&
      this.arity == EdgeLike.nrEqualingNodes(this, other)

    override protected def baseHashCode: Int = (0 /: iterator)(_ ^ _.hashCode)
  }
  protected[collection] trait EqDiHyper extends Eq {
    this: DiHyperEdgeLike[_] =>

    override protected def baseEquals(other: EdgeLike[_]) = (arity: @switch) match {
      case 2 => other.arity == 2 &&
                this._1 == other._1 &&
                this._2 == other._2
      case a => other.arity == a && 
               (other match {
                  case diHyper: DiHyperEdge[_] =>
                    this.source == diHyper.source &&
                    EdgeLike.nrEqualingNodes(this, other) == a
                  case _ => throw new IllegalArgumentException("Unexpected edge type.")
                })
    }
    
    override protected def baseHashCode = {
      var m = 4
      def mul(i: Int): Int = { m += 3; m * i }
      (0 /: iterator)((s: Int, n: Any) => s ^ mul(n.hashCode))
    }
  }
  protected[collection] trait EqUnDi extends Eq {
    this: EdgeLike[_] =>

    override protected def baseEquals(other: EdgeLike[_]) =
      other.arity == 2 &&
      (this._1 == other._1 && this._2 == other._2 ||
       this._1 == other._2 && this._2 == other._1)

    override protected def baseHashCode = (_1 ##) ^ (_2 ##)
  }
  protected[collection] trait EqDi extends Eq {
    this: DiEdgeLike[_] =>

    final protected override def baseEquals(other: EdgeLike[_]) =
      other.arity == 2 &&
      this._1 == other._1 &&
      this._2 == other._2

    override protected def baseHashCode = (23 * (_1 ##)) ^ (_2 ##)
  }
  /**
   * Template trait for directed edges.
   * 
   * Any class representing directed edges must inherit from this trait.
   * 
   * @author Peter Empen
   */
  trait DiHyperEdgeLike[+N]
      extends EdgeLike[N]
      with    EqDiHyper
  {
    @inline final override def directed = true
    /**
     * The tail of the arc, that is the end point from which the path (arrow) leads
     * to its head node(s).
     * 
     * @return the tail (start) node
     */
    @inline final def from = _1
    /**
     * Synonym for `from`.
     * 
     * @return the tail (source) node
     */
    @inline final def source = from
    /**
     * The head of the arc, that is the end point which the path (arrow) leads to.
     * 
     * @return the head (end) node
     */
    def to = _n(arity - 1) 
    /**
     * Synonym for `to`.
     * 
     * @return the target (end) node.
     */
    @inline final def target = to

    override def hasSource[M>:N](node: M) = this._1 == node
    override def hasSource[M>:N](pred: N => Boolean) = pred(this._1)

    override def hasTarget[M>:N](node: M) = (iterator drop 1) contains node
    override def hasTarget[M>:N](pred: N => Boolean) = (iterator drop 1) exists pred

    override def withSources(f: N => Unit) = f(this._1)
    override def withTargets(f: N => Unit) = (iterator drop 1) foreach f

    override protected def nodesToStringSeparator = DiEdgeLike.nodeSeparator
  }
  object DiEdgeLike {
    val nodeSeparator = "~>" 
    def unapply[N](e: DiEdgeLike[N]) = Some(e)
  }
  trait DiEdgeLike[+N]
      extends DiHyperEdgeLike[N]
      with    EqDi {
    @inline final override def to = _2

    final override def hasSource[M>:N](node: M) = this._1 == node
    final override def hasSource[M>:N](pred: N => Boolean) = pred(this._1)

    final override def hasTarget[M>:N](node: M) = this._2 == node
    final override def hasTarget[M>:N](pred: N => Boolean) = pred(this._2)

    final override def withSources(f: N => Unit) = f(this._1)
    final override def withTargets(f: N => Unit) = f(this._2)
  }
  case class EdgeException(val msg: String) extends Exception
  
  /**
   * This trait supports extending the default key of an edge with additional attributes.
   *  
   * As a default, the key - represented by `hashCode` - of an edge is made up of the
   * participating nodes.
   * Custom edges may need to expand this default key with additional attributes
   * thus enabling the definition of several edges between the same nodes (multi-edges).
   * Edges representing flight connections between airports are a typical example
   * for this requirement because their key must also include something like a flight number.
   * When defining a custom edge for a multi-graph, this trait must be mixed in.
   *     
   * @tparam N    type of the nodes
   * @author Peter Empen
   */
  trait ExtendedKey[N] extends EdgeLike[N]
  {
    /**
     * Each element in this sequence references an attribute of the custom  
     * edge which composes the key of this edge. All attributes added to this sequence
     * will be considered when calculating `equals` and `hashCode`.
     * 
     * Neither an empty sequence, nor null elements are permitted.
     */
    def keyAttributes: Seq[Any]
    override def equals(other: Any) =
      super.equals(other) && (other match {
        case that: ExtendedKey[_] => this.keyAttributes == that.keyAttributes  
        case _ => false
      })
    override def hashCode = super.hashCode + (keyAttributes map (_.## * 41) sum)  
  
    override protected def attributesToString: String
  }
  object ExtendedKey {
    def unapply[N](e: ExtendedKey[N]) = Some(e)
  }
  trait LoopFreeEdge[N] extends EdgeLike[N]
  {
    override protected def isValidCustom = {
      super.isValidCustom
      if (arity == 2) _1 != _2
      else nonLooping
    }
    override protected def isValidCustomExceptionMessage = "No loop is allowed: " + toString
  }
  object LoopFreeEdge {
    def unapply[N](e: LoopFreeEdge[N]) = Some(e)
  }
  /** Marker trait for companion objects of any kind of edge. */
  trait EdgeCompanionBase[+E[N] <: EdgeLike[N]]
  /**
   * The abstract methods of this trait must be implemented by companion objects
   * of simple (non-weighted, non-labeled) hyperedges.
   * 
   * @author Peter Empen
   */
  trait HyperEdgeCompanion[+E[N] <: EdgeLike[N]] extends EdgeCompanionBase[E] {
    def apply[N](node_1: N, node_2: N, nodes: N*): E[N]
    /** @param nodes must be of arity >= 2 */
    protected[collection] def from [N](nodes: Product): E[N]
  }
  /**
   * The abstract methods of this trait must be implemented by companion objects
   * of simple (non-weighted, non-labeled) edges.
   * 
   * @author Peter Empen
   */
  trait EdgeCompanion[+E[N] <: EdgeLike[N]] extends EdgeCompanionBase[E] {
    def apply[N](node_1: N, node_2: N): E[N]
    /** @param nodes must be of arity == 2 */
    protected[collection] def from [N](nodes: Product): E[N]
  }
  // ------------------------------------------------------------------------ *
  /** Represents an undirected hyperedge (hyperlink) in a hypergraph
   *  with unlimited number of nodes.
   * 
   * @author Peter Empen
   */
  @SerialVersionUID(50L)
  class HyperEdge[N] (override val nodes: Product)
    extends EdgeLike[N]
    with    EdgeCopy[HyperEdge]
    with    EdgeIn[N,HyperEdge]
    with    EqHyper
  {
    validate
    protected def isValidArity(size: Int) = size >= 2

    override protected[collection]
    def copy[NN](newNodes: Product) = new HyperEdge[NN](newNodes)

    /** Iterator for the nodes (end-points) of this edge.
     */
    def iterator: Iterator[N] = nodes match {
      case i: Iterable[N] => i.iterator
      case p: Product   => p.productIterator.asInstanceOf[Iterator[N]] 
    }

    override def isAt[M>:N](node: M) = iterator contains node
    override def isAt[M>:N](pred: N => Boolean) = iterator exists pred
    
    override def hasSource[M>:N](node: M) = isAt(node)
    override def hasSource[M>:N](pred: N => Boolean) = isAt(pred)

    override def hasTarget[M>:N](node: M) = isAt(node)
    override def hasTarget[M>:N](pred: N => Boolean) = isAt(pred)

    override def withSources(f: N => Unit) = iterator foreach f
    override def withTargets(f: N => Unit) = withSources(f)
  }
  /**
   * Factory for undirected hyper-edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~ node_2 ~ node_3`
   * to `HyperEdge`.
   * 
   * @author Peter Empen
   */
  object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
    def apply[N](node_1: N, node_2: N, nodes: N*) = new HyperEdge[N](
                                                    NodeProduct(node_1, node_2, nodes: _*))
    def apply[N](nodes: Iterable[N]) = new HyperEdge[N](nodes.toList)
    protected[collection]
    def from [N](nodes: Product)     = new HyperEdge[N](nodes)
    def unapplySeq[N](e: HyperEdge[N]) =
      if (e eq null) None else Some(e._1, e.nodeSeq drop 1)
  }
  /** $SHORTCUT `hyperedge match {case n1 ~~ (n2, n3) => f(n1, n2, n3)}`.
   */
  val ~~ = HyperEdge
  
  /**
   * Represents a directed edge (link) in a hypergraph with unlimited number of nodes.
   * 
   * @author Peter Empen
   */
  @SerialVersionUID(51L)
  class DiHyperEdge[N] (nodes: Product)
    extends HyperEdge[N](nodes)
    with    DiHyperEdgeLike[N]
    with    EdgeCopy[DiHyperEdge]
    with    EdgeIn[N,DiHyperEdge]
  {
    override protected[collection] def copy[NN](newNodes: Product) =
      new DiHyperEdge[NN](newNodes)
  }
  /**
   * Factory for directed hyper-edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~> node_2 ~> node_3`
   * to `DirectedHyperEdge`.
   * 
   * @author Peter Empen
   */
  object DiHyperEdge extends HyperEdgeCompanion[DiHyperEdge] {
    def apply[N]  (from: N, to_1: N, to_n: N*) = new DiHyperEdge[N](
                                                 NodeProduct(from, to_1, to_n: _*))
    def apply[N]  (nodes: Iterable[N]) = new DiHyperEdge[N](nodes.toList)
    protected[collection]
    def from [N](nodes: Product)       = new DiHyperEdge[N](nodes)
    def unapplySeq[N](e: DiHyperEdge[N]) =
      if (e eq null) None else Some(e.from, e.nodeSeq drop 1)
  }
  /** $SHORTCUT `diHyperedge match {case source ~~ (t1, t2) => f(source, t1, t2)}`.
   */
  val ~~> = DiHyperEdge
  
  /**
   * Represents an undirected edge.
   * 
   * @author Peter Empen
   */
  @SerialVersionUID(52L)
  class UnDiEdge[N] (nodes: Product)
    extends HyperEdge[N](nodes)
    with    EdgeCopy[UnDiEdge]
    with    EdgeIn[N,UnDiEdge]
    with    EqUnDi
  {
    @inline final override protected def isValidArity(size: Int) = size == 2 
    @inline final override def isHyperEdge = false

    override protected[collection] def copy[NN](newNodes: Product) =
      new UnDiEdge[NN](newNodes)

    @inline final override def size = 2
    override def iterator: Iterator[N] = new AbstractIterator[N] {
      private var count = 0
      def hasNext = count < 2
      def next: N = {
        count += 1
        (count: @switch) match {
          case 1 => _1
          case 2 => _2
          case _ => Iterator.empty.next
        } 
      }
    }
    
    override def isAt[M>:N](node: M) = this._1 == node || this._2 == node
    override def isAt[M>:N](pred: N => Boolean) = pred(this._1) || pred(this._2)    

    override def withSources(f: N => Unit) = { f(this._1); f(this._2) }
    override def withTargets(f: N => Unit) = withSources(f)
  }
  /**
   * Factory for undirected edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~ node_2` to `UnDiEdge`.
   * 
   * @author Peter Empen
   */
  object UnDiEdge extends EdgeCompanion[UnDiEdge] {
    def apply[N]  (node_1: N, node_2: N) = new UnDiEdge[N](NodeProduct(node_1, node_2))
    def apply[N]  (nodes: Tuple2[N,N])   = new UnDiEdge[N](nodes)
    protected[collection]
    def from [N](nodes: Product)         = new UnDiEdge[N](nodes)
    def unapply[N](e: UnDiEdge[N]) = if (e eq null) None else Some(e._1, e._2)
  }
  /** $SHORTCUT `edge match {case n1 ~ n2 => f(n1, n2)}`.
   */
  val ~ = UnDiEdge
  
  /**
   * Represents a directed edge (arc / arrow) connecting two nodes.
   * 
   * @author Peter Empen
   */
  @SerialVersionUID(53L)
  class DiEdge[N] (nodes: Product)
    extends UnDiEdge[N](nodes)
    with    DiEdgeLike[N]
    with    EdgeCopy[DiEdge]
    with    EdgeIn[N,DiEdge]
  {
    override protected[collection] def copy[NN](newNodes: Product) =
      new DiEdge[NN](newNodes)
  }
  /**
   * Factory for directed edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~> node_2` to `DiEdge`.
   * 
   * @author Peter Empen
   */
  object DiEdge extends EdgeCompanion[DiEdge] {
    def apply[N](from: N, to: N)     = new DiEdge[N](NodeProduct(from, to))
    def apply[N](nodes: Tuple2[N,N]) = new DiEdge[N](nodes)
    protected[collection]
    def from [N](nodes: Product)     = new DiEdge[N](nodes)
    def unapply[N](e: DiEdge[N]) = if (e eq null) None else Some(e.source, e.target)
  }
  /** $SHORTCUT `edge match {case source ~> target => f(source, target)}`.
   */
  val ~> = DiEdge
}
