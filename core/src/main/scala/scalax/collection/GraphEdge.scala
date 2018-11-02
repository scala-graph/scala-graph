package scalax.collection

import language.higherKinds
import scala.annotation.{tailrec, switch}
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.AbstractTraversable

import GraphPredef.{InnerNodeParam, OuterEdge}
// TODO import edge.LBase.LEdge

/** Container for basic edge types to be used in the context of `Graph`.
 * You will usually simply import all its members along with the members of Param:
 * {{{
 * import scalax.collection.GraphPredef._, scalax.collection.GraphEdge,_
 * }}}
 * @define SHORTCUT Allows to replace the edge object with it's shortcut like
 * @define ORDIHYPER or the source/target ends of a directed hyperedge
 * @define BAG bag that is an unordered collection of nodes with duplicates allowed
 * @author Peter Empen
 */
object GraphEdge {
  /** Template for Edges in a Graph.
   * 
   * Implementation note: Irrespective of the containing `Graph` all library-provided Edges
   * are immutable.
   * 
   * @tparam N the user type of the nodes (ends) of this edge.
   * @define CalledByValidate This function is called on every edge-instantiation
   *         by `validate` that throws EdgeException if this method returns `false`.
   * @define ISAT In case this edge is undirected this method maps to `isAt`
   * @author Peter Empen
   */
  sealed trait EdgeLike[+N] extends Iterable[N] with Eq with Serializable
  {
    /** The endpoints of this edge, in other words the nodes this edge joins.
     */
    def ends: Traversable[N]
    
    /** Iterator for the end-points of this edge, in other words the nodes sitting at this edge. */
    @deprecated("Use 'ends' instead.", "1.12.0")
    def iterator: Iterator[N] = ends.toIterator
    
    /** Sequence of the end points of this edge. */
    def nodeSeq: Seq[N] = ends.toSeq
    
    /** The n'th node with 0 <= n < arity. */
    def _n(n: Int): N
    
    /** Number of the endpoints of this edge. At least two nodes are joined. In case of
     *  a hook, the two nodes are identical. Hyperedges may link more than two nodes.
     */
    def arity: Int
    
    /** Determines whether the `arity` is valid.
     *  $CalledByValidate
     */
    protected def isValidArity: Boolean
    
    /** Ensures that none of the endpoints is a null reference.
     *  $CalledByValidate
     */
    protected def noNullEnd: Boolean
    
    /** This method may be overridden to enforce additional validation at edge creation time.
     *  Its default implementation always returns `true`.  
     *  $CalledByValidate
     */
    protected def isValidCustom: Boolean = true
    
    protected def customMsgPrefix: String = "Custom validation failed"
    
    protected class EdgeValidationException(val msg: String) extends Exception

    protected final def handleFailure(msgPrefix: String): Unit =
      throw new EdgeValidationException(s"$msgPrefix at $toString.")
    
    /** Performs basic edge validation by calling `isValidArity` and `noNullEnd`.
     *  `isValidCustom` is also called but you need to override this member to perform additional validation.
     *  This validation method needs to be called in the constructor of any edge class
     *  that directly extends or mixes in `EdgeLike`. 
     *
     *  @throws EdgeException if any of the basic validations or the additional custom validation fails.
     */
    protected final def validate(): Unit = {
      if (! isValidArity)      handleFailure("Invalid arity detected")
      else if (! noNullEnd)    handleFailure("Null endpoint detected")
      else if(! isValidCustom) handleFailure(customMsgPrefix)
    }
    
    /** Whether this edge is directed. */
    def isDirected: Boolean
    
    /** Whether this edge is undirected. */
    @inline final def isUndirected = ! isDirected
    
    /** Whether this edge's type is hyperedge meaning that it may have more than two ends. */
    def isHyperEdge: Boolean
    
    /** Whether this edge has exactly two ends. */
    @inline final def nonHyperEdge = ! isHyperEdge
    
    /** Whether this edge produces a self-loop.
     *  In case of a non-hyperedge, a loop is given if the incident nodes are equal.
     *  In case of a directed hyperedge, a loop is given if the source is equal to
     *  any of the targets.
     *  In case of an undirected hyperedge, a loop is given if any pair of incident
     *  nodes has equal nodes.
     */
    def isLooping = if (arity == 2) ends.head == ends.drop(1).head
                    else if (isDirected) ends.drop(1) exists (_ == ends.head)
                    else (MSet() ++= ends).size < arity
                    
    /** Same as `! looping`. */                
    final def nonLooping = ! isLooping
    
    /** The weight of this edge with a default of 1.
     * 
     *  Note that `weight` is normally not part of the edge key (hashCode). As a result,
     *  edges with different weights connecting the same nodes will be evaluated as equal
     *  and thus added once and only once to the graph.
     *  In case you need multi-edges based on different weights 
     *  you should either make use of a predefined key-weighted edge type such as `WDiEdge` 
     *  or define a custom edge class that mixes in `ExtendedKey` and adds `weight` to
     *  `keyAttributes`.
     */
    def weight: Double = 1
    /** The label of this edge. If `Graph`'s edge type parameter has been inferred or set
     *  to a labeled edge type all contained edges are labeled. Otherwise you should
     *  assert, for instance by calling `isLabeled`, that the edge instance is labeled
     *  before calling this method.
     * 
     *  Note that `label` is normally not part of the edge key (hashCode). As a result,
     *  edges with different labels connecting the same nodes will be evaluated as equal
     *  and thus added once and only once to the graph.
     *  In case you need multi-edges based on different labels 
     *  you should either make use of a predefined key-labeled edge type such as `LDiEdge` 
     *  or define a custom edge class that mixes in `ExtendedKey` and adds `label` to
     *  `keyAttributes`. 
     * 
     * @throws UnsupportedOperationException if the edge is non-labeled.
     */
    def label: Any =
      throw new UnsupportedOperationException("Call of label for a non-labeled edge.")

    /** `true` if this edge is labeled. See also `label`. */
    def isLabeled: Boolean = ??? 

    /** Same as `isAt`. */
    @inline final def contains[M>:N](node: M): Boolean = isAt(node)

    /** `true` if `node` is incident with this edge. */
    def isAt[M>:N](node: M): Boolean
    
    /** `true` if any end of this edge fulfills `pred`. */
    def isAt(pred: N => Boolean): Boolean
    
    /** `true` if `node` is a source of this edge. $ISAT. */
    def hasSource[M>:N](node: M): Boolean
    
    /** `true` if any source end of this edge fulfills `pred`. */
    def hasSource(pred: N => Boolean): Boolean

    /** `true` if `node` is a target of this edge. $ISAT. */
    def hasTarget[M>:N](node: M): Boolean
    
    /** `true` if any target end of this edge fulfills `pred`. */
    def hasTarget(pred: N => Boolean): Boolean
    
    /** Applies `f` to all source ends of this edge without new memory allocation. */
    def withSources[U](f: N => U): Unit
    
    /** All source ends of this edge. */
    def sources: Traversable[N] = new AbstractTraversable[N] {
      def foreach[U](f: N => U): Unit = withSources(f)
    }
    
    /** Applies `f` to the target ends of this edge without new memory allocation. */
    def withTargets[U](f: N => U): Unit
    
    /** All target ends of this edge. */
    def targets: Traversable[N] = new AbstractTraversable[N] {
      def foreach[U](f: N => U): Unit = withTargets(f)
    }

    /** `true` if<br />
     *  a) both `n1` and `n2` are at this edge for an undirected edge<br />
     *  b) `n1` is a source and `n2` a target of this edge for a directed edge. */
    def matches[M>:N](n1: M, n2: M): Boolean
    
    /** `true` if<br />
     *  a) two distinct ends of this undirected edge exist
     *     for which `p1` and `p2` hold or<br />
     *  b) `p1` holds for a source and `p2` for a target of this directed edge. */
    def matches(p1: N => Boolean, p2: N => Boolean): Boolean

    override def canEqual(that: Any): Boolean = that.isInstanceOf[EdgeLike[_]]

    override def equals(other: Any): Boolean = other match {
      case that: EdgeLike[_] => 
        (this eq that) ||
        (that canEqual this) && 
        (this.isDirected == that.isDirected) &&
        (this.isInstanceOf[Keyed] == that.isInstanceOf[Keyed]) &&
        equals(that) 
      case _ => false
    }

    /** Preconditions:
     *  `this.directed == that.directed &&`
     *  `this.isInstanceOf[Keyed] == that.isInstanceOf[Keyed]`
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
        ends.toString.patch(0, stringPrefix, stringPrefix.length) 
      else
        ends mkString nodesToStringSeparator
        
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
  
  /** This trait is to be mixed in by every class implementing EdgeLike.
   */
  trait EdgeCopy[+CC[X] <: EdgeLike[_]] {
    /** It is a prerequisite for edge-classes to implement this method. Otherwise
     *  they cannot be passed to a `Graph`.
     * 
     *  `Graph` calls this method internally to obtain a new instance of the 
     *  edge passed to `Graph` with nodes of the type of the inner class `NodeT`
     *  which itself contains the outer node. 
     */
    protected[collection] def copy[NN](newNodes: Product): CC[NN]
  }
  object EdgeLike {
    val nodeSeparator = "~" 
    protected case class Brackets(left: Char, right: Char)
    protected val curlyBraces = Brackets('{', '}')
    def unapply[N](e: EdgeLike[N]) = Some(e)
  }
  
  /** Helper object to convert edge-factory parameter-lists to tuple-n or list.
   *  
   * @author Peter Empen
   */
  object NodeProduct {
    @inline final def apply[N](node_1: N, node_2: N): Tuple2[N,N] = Tuple2(node_1, node_2)
    @inline def apply[N](node_1: N, node_2: N, nodes: N*): Product = {
      (nodes.size: @scala.annotation.switch) match {
        case 1 => Tuple3(node_1, node_2, nodes(0))
        case 2 => Tuple4(node_1, node_2, nodes(0), nodes(1))
        case 3 => Tuple5(node_1, node_2, nodes(0), nodes(1), nodes(2))
        case _ => List[N](node_1, node_2) ::: List(nodes: _*) 
      }
    }
    final def apply[N](nodes: Iterable[N]): Product = nodes match {
      case n1 :: n2 :: rest =>
        if (rest eq Nil) apply(n1, n2)
        else             apply(n1, n2, rest: _*)
      case _ =>
        val it = nodes.iterator
        val n1 = it.next
        val n2 = it.next
        if (it.hasNext) apply(n1, n2, it.toList: _*)
        else            apply(n1, n2)
    }
  }
  protected[collection] trait Keyed
  
  /** Defines how to handle the ends of hyperedges, or the source/target ends of directed hyperedges,
   *  with respect to equality. */
  sealed abstract class CollectionKind(val duplicatesAllowed: Boolean, val orderSignificant: Boolean)
  object CollectionKind {
    protected[collection] def from(duplicatesAllowed: Boolean, orderSignificant: Boolean): CollectionKind =
      if (duplicatesAllowed)
        if (orderSignificant) Sequence else Bag
      else
        throw new IllegalArgumentException("'duplicatesAllowed == false' is not supported for endpoints kind.")
    
    protected[collection] def from(s: String): CollectionKind =
      if      (s == Bag.toString)      Bag 
      else if (s == Sequence.toString) Sequence
      else throw new IllegalArgumentException(s"Unexpected representation of '$s' for endpoints kind.")
      
    protected[collection] def from(edge: EdgeLike[_]): CollectionKind =
      CollectionKind.from(true, edge.isInstanceOf[OrderedEndpoints])
      
    def unapply(kind: CollectionKind): Option[(Boolean, Boolean)] =
      Some((kind.duplicatesAllowed, kind.orderSignificant))
  }
  
  /** Marks a hyperedge, $ORDIHYPER, to handle the endpoints
   *  as an unordered collection of nodes with duplicates allowed. */
  case object Bag extends CollectionKind(true, false)
  
  /** Marks a hyperedge, $ORDIHYPER, to handle the endpoints
   *  as an ordered collection of nodes with duplicates allowed. */
  case object Sequence extends CollectionKind(true, true)
  
  /** Marks (directed) hyperedge endpoints to have a significant order. */
  protected[collection] trait OrderedEndpoints
  
  protected[collection] sealed trait Eq {
    protected def baseEquals(other: EdgeLike[_]): Boolean
    protected def baseHashCode: Int
  }
  protected[collection] object Eq {
    def nrEqualingNodes(itA: Iterator[_], itB: Iterable[_]): Int = {
      var nr = 0
      val bLen = itB.size
      val used = new Array[Boolean](bLen)
      for (a <- itA) {
        val bs = itB.iterator
        var j = 0
        while(j < bLen) {
          val b = bs.next
          if (! used(j) && a == b) {
            nr += 1
            used(j) = true
            j = bLen
          }
          j += 1
        }
      }
      nr
    }
    def equalTargets(left : EdgeLike[_], leftEnds : Traversable[_],
                     right: EdgeLike[_], rightEnds: Traversable[_], arity: Int): Boolean = {
      val thisOrdered = left .isInstanceOf[OrderedEndpoints]
      val thatOrdered = right.isInstanceOf[OrderedEndpoints]
      thisOrdered == thatOrdered && (
        if (thisOrdered) leftEnds.toSeq sameElements rightEnds.toSeq
        else Eq.nrEqualingNodes(leftEnds.toIterator, rightEnds.toIterable) == arity
      )
    } 
  }
  
  protected[collection] trait EqHyper extends Eq {
    this: AbstractHyperEdge[_] =>
    override protected def baseEquals(other: EdgeLike[_]) = {
      val (thisArity, thatArity) = (arity, other.arity)
      if (thisArity == thatArity)
        Eq.equalTargets(this, this.sources, other, other.sources, thisArity)
      else false
    }
    override protected def baseHashCode: Int = (0 /: ends)(_ ^ _.hashCode)
  }
  
  /** Equality for targets handled as a $BAG.
   *  Targets are equal if they contain the same nodes irrespective of their position.
   */
  protected[collection] trait EqDiHyper extends Eq {
    this: AbstractDiHyperEdge[_] =>

    override protected def baseEquals(other: EdgeLike[_]) = {
      val (thisArity, thatArity) = (arity, other.arity)
      if (thisArity == thatArity)
        if (thisArity == 2)
          this.sources.head == other.sources.head &&
          this.targets.head == other.targets.head
        else
          other match {
            case diHyper: AbstractDiHyperEdge[_] =>
              this.sources.head == diHyper.sources.head &&
              Eq.equalTargets(this, this.targets, other, other.targets, arity - 1)
            case _ => false
          }
      else false
    }
    
    override protected def baseHashCode = {
      var m = 4
      def mul(i: Int): Int = { m += 3; m * i }
      (0 /: ends)((s: Int, n: Any) => s ^ mul(n.hashCode))
    }
  }
  protected[collection] trait EqUnDi extends Eq {
    this: AbstractUnDiEdge[_] =>

    @inline final protected def unDiBaseEquals(n1: Any, n2: Any) =
      this._1 == n1 && this._2 == n2 ||
      this._1 == n2 && this._2 == n1
      
    override protected def baseEquals(other: EdgeLike[_]) = other match {
      case AbstractEdge(n1, n2)                      => unDiBaseEquals(n1, n2)
      case hyper: AbstractHyperEdge[_]
           if hyper.isUndirected && hyper.arity == 2 => unDiBaseEquals(hyper._n(0), hyper._n(1))
      case _                                         => false
    }

    override protected def baseHashCode = (_1.##) ^ (_2.##)
  }
  protected[collection] trait EqDi extends Eq {
    this: AbstractDiEdge[_] =>

    @inline final protected def diBaseEquals(n1: Any, n2: Any) =
      this._1 == n1 &&
      this._2 == n2
      
    final protected override def baseEquals(other: EdgeLike[_]) = other match {
      case AbstractDiEdge(source, target)                    => diBaseEquals(source, target)
      case hyper: AbstractDiHyperEdge[_] if hyper.arity == 2 => diBaseEquals(hyper.sources.head, hyper.targets.head)
      case _                                                 => false
    }

    override protected def baseHashCode = (23 * (_1.##)) ^ (_2.##)
  }
  
  /** This trait supports extending the default key of an edge with additional attributes.
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
  trait ExtendedKey[+N] extends EdgeLike[N]
  {
    /** Each element in this sequence references an attribute of the custom
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
    override def hashCode = super.hashCode + keyAttributes.map(_.## * 41).sum  
  
    override protected def attributesToString: String
  }
  object ExtendedKey {
    def unapply[N](e: ExtendedKey[N]) = Some(e)
  }
  
  trait LoopFreeEdge[+N] { this: EdgeLike[N] =>
    override protected def isValidCustom = {
      if (arity == 2) ends.head != ends.drop(1).head 
      else nonLooping
    }
    override protected def customMsgPrefix = "Unexpected loop detected"
  }
  object LoopFreeEdge {
    def unapply[N](e: LoopFreeEdge[N]) = Some(e)
  }
  
  /** Marker trait for companion objects of any kind of edge. */
  sealed trait EdgeCompanionBase[+E[N] <: EdgeLike[N]] extends Serializable
  
  /**
   * The abstract methods of this trait must be implemented by companion objects
   * of simple (non-weighted, non-labeled) hyperedges.
   */
  trait HyperEdgeCompanion[+E[N] <: EdgeLike[N]] extends EdgeCompanionBase[E] {
    def apply[N](node_1: N, node_2: N, nodes: N*)(implicit endpointsKind: CollectionKind = Bag): E[N]
    /** @param nodes must be of arity >= 2 */
    protected[collection] def from[N](nodes: Product)(implicit endpointsKind: CollectionKind): E[N]
  }
  /** The abstract methods of this trait must be implemented by companion objects
   * of simple (non-weighted, non-labeled) edges.
   */
  trait EdgeCompanion[+E[N] <: EdgeLike[N] with AbstractEdge[N]] extends EdgeCompanionBase[E] {
    def apply[N](node_1: N, node_2: N): E[N]
    /** @param nodes must be of arity == 2 */
    protected[collection] def from[N](nodes: Product): E[N]
    def unapply[N](e: E[N] @uV) = if (e eq null) None else Some(e._1, e._2)
  }
  
  // ------------------------------------------------------------------------ *
  trait AbstractHyperEdge[+N]
      extends EdgeLike[N]
      with    EqHyper {
    
    def _1: N = ends.head   
    def _2: N = ends.drop(1).head
    def _n(n: Int): N = ends.drop(n).head
    def arity: Int = ends.size

    def isDirected = false   
    def isHyperEdge = true   

    protected def isValidArity = ends.size >= 2
    protected def noNullEnd(coll: Traversable[N @uV]): Boolean = coll forall {
      case n: AnyRef => n ne null
      case _ => true
    }
 
    protected def noNullEnd = noNullEnd(ends)
      
    override def isAt[M>:N](node: M) = iterator contains node
    override def isAt(pred: N => Boolean) = ends exists pred
    
    override def hasSource[M>:N](node: M) = isAt(node)
    override def hasSource(pred: N => Boolean) = isAt(pred)

    override def hasTarget[M>:N](node: M) = isAt(node)
    override def hasTarget(pred: N => Boolean) = isAt(pred)

    override def withSources[U](f: N => U) = ends foreach f
    override def withTargets[U](f: N => U) = withSources(f)

    final protected def matches(fList: List[N => Boolean]): Boolean = {
      val it = iterator
      @tailrec def loop(checks: List[N => Boolean]): Boolean = {
        if (checks.isEmpty) true 
        else if (! it.hasNext) false
        else {
          val n = it.next
          val f = checks find (f => f(n))
          if (f.isDefined) loop(checks diff List(f.get))
          else             loop(checks)
        }
      }
      loop(fList)
    }
    override def matches[M >: N](n1: M, n2: M): Boolean =
      matches(List((n: M) => n == n1,
                   (n: M) => n == n2))
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      matches(List(p1, p2))
  }
  
  private def productToList[N](nodes: Product): List[N] = nodes match {
    case list: List[_] => list.asInstanceOf[List[N]]
    case p: Product    => p.productIterator.asInstanceOf[Iterator[N]].toList
  }
    
  /** Represents an undirected hyperedge (hyperlink) in a hypergraph
   *  with unlimited number of nodes.
   */
  @SerialVersionUID(51)
  class HyperEdge[+N] (override val ends: Traversable[N])
      extends AbstractHyperEdge[N]
      with    EdgeCopy [HyperEdge]
      with    OuterEdge[N,HyperEdge] {
    validate()
    
    override protected[collection] def copy[NN](newNodes: Product) =
      if (this.isInstanceOf[OrderedEndpoints]) new HyperEdge[NN](productToList(newNodes)) with OrderedEndpoints
      else                                     new HyperEdge[NN](productToList(newNodes))
  }
    
  /** Factory for undirected hyper-edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~ node_2 ~ node_3`
   * to `HyperEdge`.
   */
  object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
    def apply[N](node_1: N, node_2: N, nodes: N*)(implicit endpointsKind: CollectionKind = Bag): HyperEdge[N] =
      from(NodeProduct(node_1, node_2, nodes: _*))
    def apply[N](nodes: Traversable[N])(implicit endpointsKind: CollectionKind): HyperEdge[N] =
      from(nodes.toList)
    protected[collection] def from [N](nodes: Product)(implicit endpointsKind: CollectionKind): HyperEdge[N] =
      if (endpointsKind.orderSignificant) new HyperEdge[N](productToList(nodes)) with OrderedEndpoints
      else                                new HyperEdge[N](productToList(nodes))
    def unapplySeq[N](e: HyperEdge[N]) =
      if (e eq null) None else Some(e._1, e.nodeSeq drop 1)
  }
  /** $SHORTCUT `hyperedge match {case n1 ~~ (n2, n3) => f(n1, n2, n3)}`.
   */
  val ~~ = HyperEdge
  
  trait AbstractDiHyperEdge[+N]
      extends AbstractHyperEdge[N]
      with    EqDiHyper {
    
    override def _n(n: Int): N = ends.drop(n).head
    
    override def arity: Int = sources.size + targets.size
		override protected def isValidArity = sources.toIterator.hasNext && targets.toIterator.hasNext
		override protected def noNullEnd = noNullEnd(sources) && noNullEnd(targets)
    
		@inline final override def isDirected = true

    override def hasSource[M>:N](node: M) = this._1 == node
    override def hasSource(pred: N => Boolean) = pred(this._1)

    override def hasTarget[M>:N](node: M) = targets exists (_ == node)
    override def hasTarget(pred: N => Boolean) = targets exists pred

    override def withSources[U](f: N => U) = f(sources.head)
    override def withTargets[U](f: N => U) = (ends drop 1) foreach f

    override def matches[M >: N](n1: M, n2: M): Boolean =
      sources.exists(_ == n1) && targets.exists(_ == n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      (sources exists p1) && (targets exists p2)

    override protected def nodesToStringSeparator = AbstractDiEdge.nodeSeparator
  }
  
  /** Represents a directed edge in a hypergraph with a single source and an unlimited number
   *  of taget nodes. Target nodes are handled as a $BAG.
   */
  // 'extends HyperEdge' should be dropped but is currently needed for inference in mixed graphs
  @SerialVersionUID(52)
  class DiHyperEdge[+N] (override val sources: Traversable[N], override val targets: Traversable[N])
      extends HyperEdge[N](sources ++ targets)
      with    AbstractDiHyperEdge[N]
      with    EdgeCopy       [DiHyperEdge]
      with    OuterEdge      [N,DiHyperEdge] {
    validate()
    
    override protected[collection] def copy[NN](newNodes: Product): DiHyperEdge[NN] = {
      if (this.isInstanceOf[OrderedEndpoints]) new DiHyperEdge[NN](fromFirst(newNodes), fromSecond(newNodes)) with OrderedEndpoints
      else                                     new DiHyperEdge[NN](fromFirst(newNodes), fromSecond(newNodes))
    }
  }
  
  private def fromFirst [N](nodes: Product): Traversable[N] = (nodes match {
    case list: List[_] => list.head.asInstanceOf[N]
    case p: Product    => p.productElement(0).asInstanceOf[N]
  }) :: Nil
  private def fromSecond[N](nodes: Product): Traversable[N] = nodes match {
    case list: List[_] => list.tail.asInstanceOf[List[N]]
    case p: Product    => p.productIterator.drop(1).asInstanceOf[Iterator[N]].toList
  }

  /**
   * Factory for directed hyper-edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~> node_2 ~> node_3`
   * to `DirectedHyperEdge`.
   */
  object DiHyperEdge extends HyperEdgeCompanion[DiHyperEdge] {
    def apply[N]  (from: N, to_1: N, to_n: N*)(implicit targetsKind: CollectionKind = Bag): DiHyperEdge[N] =
      DiHyperEdge.from(NodeProduct(from, to_1, to_n: _*))
    def apply[N]  (nodes: Traversable[N])(implicit targetsKind: CollectionKind): DiHyperEdge[N] =
      DiHyperEdge.from(nodes.toList)
    protected[collection] def from [N](nodes: Product)(implicit targetsKind: CollectionKind): DiHyperEdge[N] =
      if (targetsKind.orderSignificant) new DiHyperEdge[N](fromFirst(nodes), fromSecond(nodes)) with OrderedEndpoints
      else                              new DiHyperEdge[N](fromFirst(nodes), fromSecond(nodes))
    def unapplySeq[N](e: DiHyperEdge[N]) =
      if (e eq null) None else Some(e.sources.head, e.targets.to[Vector])
  }
  /** $SHORTCUT `diHyperedge match {case source ~~> (t1, t2) => f(source, t1, t2)}`.
   */
  val ~~> = DiHyperEdge

  trait AbstractEdge[+N] { this: EdgeLike[N] =>

    def _1: N   
    def _2: N
    
    // the following five methods should be made final as soon as DiEdge no more extends UnDiEdge
    @inline override def arity: Int = 2
		@inline override protected def isValidArity = true
		
    @inline override def isHyperEdge = false
    
    override protected def noNullEnd: Boolean = {
      def notNull(n: N) = n match {
        case r: AnyRef => r ne null
        case _         => true
      }
      notNull(_1) && notNull(_2)
    }

    @inline override def size = 2
    
    def ends = new AbstractTraversable[N] {
      def foreach[U](f: N => U): Unit = { f(_1); f(_2) }
    }

    override def isAt[M>:N](node: M) = this._1 == node || this._2 == node
    override def isAt(pred: N => Boolean) = pred(this._1) || pred(this._2)    
  }
  object AbstractEdge {
    def unapply[N](e: AbstractEdge[N] @uV) = if (e eq null) None else Some(e._1, e._2)
  }
  
  trait AbstractUnDiEdge[+N]
      extends AbstractHyperEdge[N]
      with    AbstractEdge[N]
      with    EqUnDi {
    
    def node_1: N
    def node_2: N
    
    @inline final override def _1: N = node_1   
    @inline final override def _2: N = node_2
    
    override def _n(n: Int): N = (n: @switch) match {
      case 0 => node_1
      case 1 => node_2
      case _ => throw new IndexOutOfBoundsException
    }
    
    override def isDirected = false   

    override def withSources[U](f: N => U) = { f(this.node_1); f(this.node_2) }
    override def withTargets[U](f: N => U) = withSources(f)

    override def matches[M >: N](n1: M, n2: M): Boolean = unDiBaseEquals(n1, n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
        (p1(this._1) && p2(this._2) ||
         p1(this._2) && p2(this._1)  )
  }
  object AbstractUnDiEdge {
    val nodeSeparator = "~" 
    def unapply[N](e: AbstractDiEdge[N]) = Some(e)
  }

  private def first [N](nodes: Product): N = nodes.productElement(0).asInstanceOf[N]
  private def second[N](nodes: Product): N = nodes.productElement(1).asInstanceOf[N]

  /** Represents an undirected edge.
   */
  @SerialVersionUID(53)
  class UnDiEdge[+N] (val node_1: N, val node_2: N)
      extends HyperEdge[N](node_1 :: node_2 :: Nil)
      with    AbstractUnDiEdge[N]
      with    EdgeCopy [UnDiEdge]
      with    OuterEdge[N,UnDiEdge] {
    validate()
    
    override final val ends = super[AbstractUnDiEdge].ends
    
    override protected[collection] def copy[NN](newNodes: Product): UnDiEdge[NN] =
        new UnDiEdge[NN](first[NN](newNodes), second[NN](newNodes))
  }
  /** Factory for undirected edges.
   *  `GraphPredef` also supports implicit conversion from `node_1 ~ node_2` to `UnDiEdge`.
   */
  object UnDiEdge extends EdgeCompanion[UnDiEdge] {
    def apply[N](node_1: N, node_2: N) = new UnDiEdge[N](node_1, node_2)
    def apply[N](nodes: Tuple2[N,N])   = new UnDiEdge[N](nodes._1, nodes._2)
    protected[collection] def from [N](nodes: Product) = new UnDiEdge[N](first[N](nodes), second[N](nodes))
  }
  /** $SHORTCUT `edge match {case n1 ~ n2 => f(n1, n2)}`.
   */
  val ~ = UnDiEdge
  
  trait AbstractDiEdge[+N]
      extends AbstractDiHyperEdge[N]
      with    AbstractEdge[N]
      with    EqDi {
    
    def source: N
    def target: N

    final override def hasSource[M>:N](node: M) = this._1 == node
    final override def hasSource(pred: N => Boolean) = pred(this._1)

    final override def hasTarget[M>:N](node: M) = this._2 == node
    final override def hasTarget(pred: N => Boolean) = pred(this._2)

    final override def withTargets[U](f: N => U) = f(this._2)
    final override def withSources[U](f: N => U) = f(this._1)
    
    override def matches[M >: N](n1: M, n2: M): Boolean = diBaseEquals(n1, n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      p1(this._1) && p2(this._2)
  }
  object AbstractDiEdge {
    val nodeSeparator = "~>" 
    def unapply[N](e: AbstractDiEdge[N]) = Some(e)
  }

  /** Represents a directed edge (arc / arrow) connecting two nodes.
   */
  // 'extends UnDiEdge' should be dropped but is currently needed for inference in mixed graphs
  @SerialVersionUID(54)
  class DiEdge[+N] (val source: N, val target: N)
      extends UnDiEdge[N](source, target)
      with    AbstractDiEdge[N]
      with    EdgeCopy  [DiEdge]
      with    OuterEdge [N,DiEdge] {
    validate()
    
    override protected[collection] def copy[NN](newNodes: Product): DiEdge[NN] =
      new DiEdge[NN](first[NN](newNodes), second[NN](newNodes))
  }
  /** Factory for directed edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~> node_2` to `DiEdge`.
   * 
   * @author Peter Empen
   */
  object DiEdge extends EdgeCompanion[DiEdge] {
    def apply[N](from: N, to: N)     = new DiEdge[N](from, to)
    def apply[N](nodes: Tuple2[N,N]) = new DiEdge[N](nodes._1, nodes._2)
    protected[collection] def from [N](nodes: Product) = new DiEdge[N](first[N](nodes), second[N](nodes))
  }
  /** $SHORTCUT `edge match {case source ~> target => f(source, target)}`.
   */
  val ~> = DiEdge
}
