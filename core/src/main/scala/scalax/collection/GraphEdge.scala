package scalax.collection

import language.higherKinds
import scala.annotation.{switch, tailrec}
import scala.collection.AbstractIterator
import GraphPredef.OuterEdge

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
  sealed trait EdgeLike[+N] extends Iterable[N] with Eq with Serializable {

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

    /** The first node. Same as _n(0).
      */
    @inline final def _1: N = nodes.productElement(0).asInstanceOf[N]

    /** The second node. Same as _n(1).
      */
    def _2: N = nodes match {
      case i: Iterable[N] => i.drop(1).head
      case p: Product     => nodes.productElement(1).asInstanceOf[N]
    }

    /** The n'th node with 0 <= n < arity.
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

    /** Number of nodes linked by this Edge. At least two nodes are linked. In case of
      * a hook, the two nodes are identical. Hyperedges may link more than two nodes.
      */
    final def arity = nodes match {
      case i: Iterable[N] => i.size
      case p: Product     => nodes.productArity
    }

    /** A function to determine whether the `arity` of the passed `Product`
      * of nodes (that is the number of edge ends) is valid.
      * $CalledByValidate
      */
    protected def isValidArity(size: Int): Boolean

    /** This method may be overridden to enforce additional validation at edge
      * creation time. Be careful to call `super.isValidCustom` when overriding.
      * $CalledByValidate
      */
    protected def isValidCustom                 = true
    protected def isValidCustomExceptionMessage = "Custom validation failed: " + toString
    protected class EdgeException(val msg: String) extends Exception

    /** Performs basic, inevitable edge validation. Among others, ensures
      * that `nodes ne null` and no edge end `eq null`.
      *
      * This validation method must be called in the constructor of any edge class
      * that directly extends or mixes in `EdgeLike`. To perform additional custom
      * validation `isValidCustom` is to be overridden.
      *
      *  @throws EdgeException if any of the basic validations or of eventually
      *  supplied additional validations fails.
      */
    final protected def validate {
      nodes match {
        case r: AnyRef if r eq null =>
          throw new EdgeException(s"null node in: $toString")
        case _ =>
      }
      val ar = arity
      if (!(ar >= 2 && isValidArity(ar)))
        throw new EdgeException("Invalid arity: " + ar + ": " + toString)
      if (!isValidCustom)
        throw new EdgeException(isValidCustomExceptionMessage)
    }

    /** `true` if this edge is directed. */
    def directed = false

    /** Same as `directed`. */
    @inline final def isDirected = directed

    /** `true` if this edge is undirected. */
    @inline final def undirected = !directed

    /** Same as `undirected`. */
    @inline final def isUndirected = undirected

    /** `true` if this is a hyperedge that is it may have more than two ends. */
    def isHyperEdge = true

    /** `true` if this edge has exactly two ends. */
    @inline final def nonHyperEdge = !isHyperEdge

    /** `true` if this edge produces a self-loop.
      * In case of a non-hyperedge, a loop is given if the incident nodes are equal.
      * In case of a directed hyperedge, a loop is given if the source is equal to
      * any of the targets.
      * In case of an undirected hyperedge, a loop is given if any pair of incident
      * nodes has equal nodes.
      */
    def isLooping = if (arity == 2) _1 == _2
    else if (directed) iterator.drop(1) exists (_ == _1)
    else (MSet() ++= iterator).size < arity

    /** Same as `! looping`. */
    final def nonLooping = !isLooping

    /** The weight of this edge with a default of 1.
      *
      * Note that `weight` is normally not part of the edge key (hashCode). As a result,
      * edges with different weights connecting the same nodes will be evaluated as equal
      * and thus added once and only once to the graph.
      * In case you need multi-edges based on different weights
      * you should either make use of a predefined key-weighted edge type such as `WDiEdge`
      * or define a custom edge class that mixes in `ExtendedKey` and adds `weight` to
      * `keyAttributes`.
      */
    def weight: Double = 1

    /** The label of this edge. If `Graph`'s edge type parameter has been inferred or set
      * to a labeled edge type all contained edges are labeled. Otherwise you should
      * assert, for instance by calling `isLabeled`, that the edge instance is labeled
      * before calling this method.
      *
      * Note that `label` is normally not part of the edge key (hashCode). As a result,
      * edges with different labels connecting the same nodes will be evaluated as equal
      * and thus added once and only once to the graph.
      * In case you need multi-edges based on different labels
      * you should either make use of a predefined key-labeled edge type such as `LDiEdge`
      * or define a custom edge class that mixes in `ExtendedKey` and adds `label` to
      * `keyAttributes`.
      *
      * @throws UnsupportedOperationException if the edge is non-labeled.
      */
    def label: Any =
      throw new UnsupportedOperationException("Call of label for a non-labeled edge.")

    /** `true` if this edge is labeled. See also `label`. */
    def isLabeled = ??? // this.isInstanceOf[edge.LBase.LEdge[N]]

    /** Same as `isAt`. */
    @inline final def contains[M >: N](node: M): Boolean = isAt(node)

    /** `true` if `node` is incident with this edge. */
    def isAt[M >: N](node: M): Boolean

    /** `true` if any end of this edge fulfills `pred`. */
    def isAt(pred: N => Boolean): Boolean

    /** `true` if `node` is a source of this edge. $ISAT. */
    def hasSource[M >: N](node: M): Boolean

    /** `true` if any source end of this edge fulfills `pred`. */
    def hasSource(pred: N => Boolean): Boolean

    /** `true` if `node` is a target of this edge. $ISAT. */
    def hasTarget[M >: N](node: M): Boolean

    /** `true` if any target end of this edge fulfills `pred`. */
    def hasTarget(pred: N => Boolean): Boolean

    /** Applies `f` to all source ends of this edge without new memory allocation. */
    def withSources[U](f: N => U): Unit

    /** All source ends of this edge. */
    def sources: Iterable[N] = new Iterable[N] {
      override def foreach[U](f: N => U): Unit = withSources(f)
      override def iterator = ???
    }

    /** Applies `f` to the target ends of this edge without new memory allocation. */
    def withTargets[U](f: N => U): Unit

    /** All target ends of this edge. */
    def targets: Iterable[N] = new Iterable[N] {
      override def foreach[U](f: N => U): Unit = withTargets(f)
      override def iterator = ???
    }

    /** `true` if<br />
      *  a) both `n1` and `n2` are at this edge for an undirected edge<br />
      *  b) `n1` is a source and `n2` a target of this edge for a directed edge. */
    def matches[M >: N](n1: M, n2: M): Boolean

    /** `true` if<br />
      *  a) two distinct ends of this undirected edge exist
      *     for which `p1` and `p2` hold or<br />
      *  b) `p1` holds for a source and `p2` for a target of this directed edge. */
    def matches(p1: N => Boolean, p2: N => Boolean): Boolean

    def canEqual(that: Any): Boolean = that.isInstanceOf[EdgeLike[_]]

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
    override def stringPrefix                  = "Nodes"
    protected def nodesToStringWithParenthesis = false
    protected def nodesToStringSeparator       = EdgeLike.nodeSeparator
    protected def nodesToString =
      if (nodesToStringWithParenthesis)
        nodes match {
          case it: Iterable[N] => ??? //it.toString.patch(0, stringPrefix, it.stringPrefix.length)
          case _               => stringPrefix + nodes.toString
        } else
        iterator mkString nodesToStringSeparator
    protected def attributesToString      = ""
    protected def toStringWithParenthesis = false
    protected def brackets                = EdgeLike.curlyBraces
    override def toString = {
      val attr          = attributesToString
      val woParenthesis = nodesToString + (if (attr.length > 0) attr else "")
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
      * they cannot be passed to a `Graph`.
      *
      * `Graph` calls this method internally to obtain a new instance of the
      * edge passed to `Graph` with nodes of the type of the inner class `NodeT`
      * which itself contains the outer node.
      */
    protected[collection] def copy[NN](newNodes: Product): CC[NN]
  }
  object EdgeLike {
    val nodeSeparator = "~"
    protected case class Brackets(left: Char, right: Char)
    protected val curlyBraces      = Brackets('{', '}')
    def unapply[N](e: EdgeLike[N]) = Some(e)
  }

  /** Helper object to convert edge-factory parameter-lists to tuple-n or list.
    *
    * @author Peter Empen
    */
  object NodeProduct {
    @inline final def apply[N](node_1: N, node_2: N): (N, N) = (node_1, node_2)
    @inline def apply[N](node_1: N, node_2: N, nodes: N*): Product =
      (nodes.size: @scala.annotation.switch) match {
        case 1 => (node_1, node_2, nodes(0))
        case 2 => (node_1, node_2, nodes(0), nodes(1))
        case 3 => (node_1, node_2, nodes(0), nodes(1), nodes(2))
        case _ => ??? //new NodeProduct(Array(node_1, node_2, nodes???))
      }
    final def apply[N](nodes: Iterable[N]): Product = nodes match {
      case n1 :: n2 :: rest =>
        if (rest eq Nil) apply(n1, n2)
        else apply(n1, n2, rest: _*)
      case _ =>
        val it = nodes.iterator
        val n1 = it.next
        val n2 = it.next
        if (it.hasNext) apply(n1, n2, it.toList: _*)
        else apply(n1, n2)
    }
  }
  class NodeProduct(elems: Array[Any]) extends Product {
    override def productArity = elems.length
    override def productElement(n: Int) = elems(n)
    override def canEqual(that: Any) = ???
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
      if (s == Bag.toString) Bag
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

  sealed protected[collection] trait Eq {
    protected def baseEquals(other: EdgeLike[_]): Boolean
    protected def baseHashCode: Int
  }
  protected[collection] object Eq {
    def nrEqualingNodes(itA: Iterator[_], itB: Iterable[_]): Int = {
      var nr   = 0
      val bLen = itB.size
      val used = new Array[Boolean](bLen)
      for (a <- itA) {
        val bs = itB.iterator
        var j  = 0
        while (j < bLen) {
          val b = bs.next
          if (!used(j) && a == b) {
            nr += 1
            used(j) = true
            j = bLen
          }
          j += 1
        }
      }
      nr
    }
    def equalTargets(left: EdgeLike[_],
                     leftEnds: Traversable[_],
                     right: EdgeLike[_],
                     rightEnds: Traversable[_],
                     arity: Int): Boolean = {
      val thisOrdered = left.isInstanceOf[OrderedEndpoints]
      val thatOrdered = right.isInstanceOf[OrderedEndpoints]
      thisOrdered == thatOrdered && (
        if (thisOrdered) leftEnds.toSeq sameElements rightEnds.toSeq
        else Eq.nrEqualingNodes(leftEnds.iterator, rightEnds) == arity
      )
    }
  }

  protected[collection] trait EqHyper extends Eq {
    this: EdgeLike[_] =>
    override protected def baseEquals(other: EdgeLike[_]) = {
      val (thisArity, thatArity) = (arity, other.arity)
      if (thisArity == thatArity)
        Eq.equalTargets(this, this.sources, other, other.sources, thisArity)
      else false
    }
    override protected def baseHashCode: Int = (0 /: iterator)(_ ^ _.hashCode)
  }

  /** Equality for targets handled as a $BAG.
    *  Targets are equal if they contain the same nodes irrespective of their position. */
  protected[collection] trait EqDiHyper extends Eq {
    this: DiHyperEdgeLike[_] =>

    override protected def baseEquals(other: EdgeLike[_]) = {
      val (thisArity, thatArity) = (arity, other.arity)
      if (thisArity == thatArity)
        if (thisArity == 2)
          this._1 == other._1 &&
          this._2 == other._2
        else
          other match {
            case diHyper: DiHyperEdgeLike[_] =>
              this.source == diHyper.source &&
                Eq.equalTargets(this, this.targets, other, other.targets, arity - 1)
            case _ => false
          } else false
    }

    override protected def baseHashCode = {
      var m                = 4
      def mul(i: Int): Int = { m += 3; m * i }
      iterator.foldLeft(0)((s: Int, n: Any) => s ^ mul(n.hashCode))
    }
  }
  protected[collection] trait EqUnDi extends Eq {
    this: EdgeLike[_] =>

    @inline final protected def unDiBaseEquals(n1: Any, n2: Any) =
      this._1 == n1 && this._2 == n2 ||
        this._1 == n2 && this._2 == n1

    override protected def baseEquals(other: EdgeLike[_]) =
      other.arity == 2 && unDiBaseEquals(other._1, other._2)

    override protected def baseHashCode = (_1.##) ^ (_2.##)
  }
  protected[collection] trait EqDi extends Eq {
    this: DiEdgeLike[_] =>

    @inline final protected def diBaseEquals(n1: Any, n2: Any) =
      this._1 == n1 &&
        this._2 == n2

    final override protected def baseEquals(other: EdgeLike[_]) =
      other.arity == 2 && diBaseEquals(other._1, other._2)

    override protected def baseHashCode = (23 * (_1.##)) ^ (_2.##)
  }

  /** Template trait for directed edges.
    *
    * Any class representing directed edges must inherit from this trait.
    *
    * @author Peter Empen
    */
  trait DiHyperEdgeLike[+N] extends EdgeLike[N] with EqDiHyper {
    @inline final override def directed = true

    /** Synonym for `source`. */
    @inline final def from = source

    /** The single source node of this directed edge. */
    @inline final def source = _1

    /** Synonym for `target`. */
    def to = target

    /** The target node for a directed edge;
      *  one of the target nodes for a directed hyperedge. */
    @inline final def target = _2

    override def hasSource[M >: N](node: M)    = this._1 == node
    override def hasSource(pred: N => Boolean) = pred(this._1)

    override def hasTarget[M >: N](node: M)    = targets exists (_ == node)
    override def hasTarget(pred: N => Boolean) = targets exists pred

    override def withSources[U](f: N => U) = f(this._1)
    override def withTargets[U](f: N => U) = (iterator drop 1) foreach f

    override def matches[M >: N](n1: M, n2: M): Boolean =
      source == n1 && (targets exists (_ == n2))
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      p1(source) && (targets exists p2)

    override protected def nodesToStringSeparator = DiEdgeLike.nodeSeparator
  }
  trait DiEdgeLike[+N] extends DiHyperEdgeLike[N] with EqDi {
    @inline final override def to = _2

    final override def hasSource[M >: N](node: M)    = this._1 == node
    final override def hasSource(pred: N => Boolean) = pred(this._1)

    final override def hasTarget[M >: N](node: M)    = this._2 == node
    final override def hasTarget(pred: N => Boolean) = pred(this._2)

    final override def withTargets[U](f: N => U) = f(this._2)
    final override def withSources[U](f: N => U) = f(this._1)

    override def matches[M >: N](n1: M, n2: M): Boolean = diBaseEquals(n1, n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      p1(this._1) && p2(this._2)
  }
  object DiEdgeLike {
    val nodeSeparator                = "~>"
    def unapply[N](e: DiEdgeLike[N]) = Some(e)
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
  trait ExtendedKey[+N] extends EdgeLike[N] {

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
        case _                    => false
      })
    override def hashCode = super.hashCode + keyAttributes.map(_.## * 41).sum

    override protected def attributesToString: String
  }
  object ExtendedKey {
    def unapply[N](e: ExtendedKey[N]) = Some(e)
  }
  trait LoopFreeEdge[+N] extends EdgeLike[N] {
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
  trait EdgeCompanionBase[+E[N] <: EdgeLike[N]] extends Serializable

  /** The abstract methods of this trait must be implemented by companion objects
    * of simple (non-weighted, non-labeled) hyperedges.
    *
    * @author Peter Empen
    */
  trait HyperEdgeCompanion[+E[N] <: EdgeLike[N]] extends EdgeCompanionBase[E] {
    def apply[N](node_1: N, node_2: N, nodes: N*)(implicit endpointsKind: CollectionKind = Bag): E[N]

    /** @param nodes must be of arity >= 2 */
    protected[collection] def from[N](nodes: Product)(implicit endpointsKind: CollectionKind): E[N]
  }

  /** The abstract methods of this trait must be implemented by companion objects
    * of simple (non-weighted, non-labeled) edges.
    *
    * @author Peter Empen
    */
  trait EdgeCompanion[+E[N] <: EdgeLike[N]] extends EdgeCompanionBase[E] {
    def apply[N](node_1: N, node_2: N): E[N]

    /** @param nodes must be of arity == 2 */
    protected[collection] def from[N](nodes: Product): E[N]
  }
  // ------------------------------------------------------------------------ *
  /** Represents an undirected hyperedge (hyperlink) in a hypergraph
    *  with unlimited number of nodes.
    *
    * @author Peter Empen
    */
  @SerialVersionUID(50L)
  class HyperEdge[+N](override val nodes: Product)
      extends EdgeLike[N]
      with EdgeCopy[HyperEdge]
      with OuterEdge[N, HyperEdge]
      with EqHyper {
    validate
    protected def isValidArity(size: Int) = size >= 2

    override protected[collection] def copy[NN](newNodes: Product) =
      if (this.isInstanceOf[OrderedEndpoints]) new HyperEdge[NN](newNodes) with OrderedEndpoints
      else new HyperEdge[NN](newNodes)

    /** Iterator for the nodes (end-points) of this edge.
      */
    def iterator: Iterator[N] = nodes match {
      case i: Iterable[N] => i.iterator
      case p: Product     => p.productIterator.asInstanceOf[Iterator[N]]
    }

    override def isAt[M >: N](node: M)    = iterator contains node
    override def isAt(pred: N => Boolean) = iterator exists pred

    override def hasSource[M >: N](node: M)    = isAt(node)
    override def hasSource(pred: N => Boolean) = isAt(pred)

    override def hasTarget[M >: N](node: M)    = isAt(node)
    override def hasTarget(pred: N => Boolean) = isAt(pred)

    override def withSources[U](f: N => U) = iterator foreach f
    override def withTargets[U](f: N => U) = withSources(f)

    final protected def matches(fList: List[N => Boolean]): Boolean = {
      val it = iterator
      @tailrec def loop(checks: List[N => Boolean]): Boolean =
        if (checks.isEmpty) true
        else if (!it.hasNext) false
        else {
          val n = it.next
          val f = checks find (f => f(n))
          if (f.isDefined) loop(checks diff List(f.get))
          else loop(checks)
        }
      loop(fList)
    }
    override def matches[M >: N](n1: M, n2: M): Boolean =
      matches(List((n: M) => n == n1, (n: M) => n == n2))
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      matches(List(p1, p2))
  }

  /** Factory for undirected hyper-edges.
    * `GraphPredef` also supports implicit conversion from `node_1 ~ node_2 ~ node_3`
    * to `HyperEdge`.
    */
  object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
    def apply[N](node_1: N, node_2: N, nodes: N*)(implicit endpointsKind: CollectionKind = Bag): HyperEdge[N] =
      from(NodeProduct(node_1, node_2, nodes: _*))
    def apply[N](nodes: Iterable[N])(implicit endpointsKind: CollectionKind): HyperEdge[N] =
      from(NodeProduct(nodes))
    protected[collection] def from[N](nodes: Product)(implicit endpointsKind: CollectionKind): HyperEdge[N] =
      if (endpointsKind.orderSignificant) new HyperEdge[N](nodes) with OrderedEndpoints
      else new HyperEdge[N](nodes)
    def unapplySeq[N](e: HyperEdge[N]) =
      if (e eq null) None else Some(e._1, e.nodeSeq drop 1)
  }

  /** $SHORTCUT `hyperedge match {case n1 ~~ (n2, n3) => f(n1, n2, n3)}`.
    */
  val ~~ = HyperEdge

  /** Represents a directed edge in a hypergraph with a single source and an unlimited number
    *  of taget nodes. Target nodes are handled as a $BAG.
    */
  @SerialVersionUID(51L)
  class DiHyperEdge[+N](nodes: Product)
      extends HyperEdge[N](nodes)
      with DiHyperEdgeLike[N]
      with EdgeCopy[DiHyperEdge]
      with OuterEdge[N, DiHyperEdge] {
    override protected[collection] def copy[NN](newNodes: Product): DiHyperEdge[NN] =
      if (this.isInstanceOf[OrderedEndpoints]) new DiHyperEdge[NN](newNodes) with OrderedEndpoints
      else new DiHyperEdge[NN](newNodes)
  }

  /** Factory for directed hyper-edges.
    * `GraphPredef` also supports implicit conversion from `node_1 ~> node_2 ~> node_3`
    * to `DirectedHyperEdge`.
    *
    * @author Peter Empen
    */
  object DiHyperEdge extends HyperEdgeCompanion[DiHyperEdge] {
    def apply[N](from: N, to_1: N, to_n: N*)(implicit targetsKind: CollectionKind = Bag): DiHyperEdge[N] =
      DiHyperEdge.from(NodeProduct(from, to_1, to_n: _*))
    def apply[N](nodes: Iterable[N])(implicit targetsKind: CollectionKind): DiHyperEdge[N] =
      DiHyperEdge.from(NodeProduct(nodes))
    protected[collection] def from[N](nodes: Product)(implicit targetsKind: CollectionKind): DiHyperEdge[N] =
      if (targetsKind.orderSignificant) new DiHyperEdge[N](nodes) with OrderedEndpoints
      else new DiHyperEdge[N](nodes)
    def unapplySeq[N](e: DiHyperEdge[N]) =
      if (e eq null) None else Some(e.from, e.nodeSeq drop 1)
  }

  /** $SHORTCUT `diHyperedge match {case source ~~> (t1, t2) => f(source, t1, t2)}`.
    */
  val ~~> = DiHyperEdge

  /** Represents an undirected edge.
    *
    * @author Peter Empen
    */
  @SerialVersionUID(52L)
  class UnDiEdge[+N](nodes: Product)
      extends HyperEdge[N](nodes)
      with EdgeCopy[UnDiEdge]
      with OuterEdge[N, UnDiEdge]
      with EqUnDi {
    @inline final override protected def isValidArity(size: Int) = size == 2
    @inline final override def isHyperEdge                       = false

    override protected[collection] def copy[NN](newNodes: Product) =
      new UnDiEdge[NN](newNodes)

    @inline final override def size = 2
    override def iterator: Iterator[N] = new AbstractIterator[N] {
      private var count = 0
      def hasNext       = count < 2
      def next: N = {
        count += 1
        (count: @switch) match {
          case 1 => _1
          case 2 => _2
          case _ => Iterator.empty.next
        }
      }
    }

    override def isAt[M >: N](node: M)    = this._1 == node || this._2 == node
    override def isAt(pred: N => Boolean) = pred(this._1) || pred(this._2)

    override def withSources[U](f: N => U) = { f(this._1); f(this._2) }
    override def withTargets[U](f: N => U) = withSources(f)

    override def matches[M >: N](n1: M, n2: M): Boolean = unDiBaseEquals(n1, n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      (p1(this._1) && p2(this._2) ||
        p1(this._2) && p2(this._1))
  }

  /** Factory for undirected edges.
    * `GraphPredef` also supports implicit conversion from `node_1 ~ node_2` to `UnDiEdge`.
    *
    * @author Peter Empen
    */
  object UnDiEdge extends EdgeCompanion[UnDiEdge] {
    def apply[N](node_1: N, node_2: N)                = new UnDiEdge[N](NodeProduct(node_1, node_2))
    def apply[N](nodes: Tuple2[N, N])                 = new UnDiEdge[N](nodes)
    protected[collection] def from[N](nodes: Product) = new UnDiEdge[N](nodes)
    def unapply[N](e: UnDiEdge[N])                    = if (e eq null) None else Some(e._1, e._2)
  }

  /** $SHORTCUT `edge match {case n1 ~ n2 => f(n1, n2)}`.
    */
  val ~ = UnDiEdge

  /** Represents a directed edge (arc / arrow) connecting two nodes.
    *
    * @author Peter Empen
    */
  @SerialVersionUID(53L)
  class DiEdge[+N](nodes: Product)
      extends UnDiEdge[N](nodes)
      with DiEdgeLike[N]
      with EdgeCopy[DiEdge]
      with OuterEdge[N, DiEdge] {
    override protected[collection] def copy[NN](newNodes: Product) =
      new DiEdge[NN](newNodes)
  }

  /** Factory for directed edges.
    * `GraphPredef` also supports implicit conversion from `node_1 ~> node_2` to `DiEdge`.
    *
    * @author Peter Empen
    */
  object DiEdge extends EdgeCompanion[DiEdge] {
    def apply[N](from: N, to: N)                      = new DiEdge[N](NodeProduct(from, to))
    def apply[N](nodes: (N, N))                       = new DiEdge[N](nodes)
    protected[collection] def from[N](nodes: Product) = new DiEdge[N](nodes)
    def unapply[N](e: DiEdge[N])                      = if (e eq null) None else Some(e.source, e.target)
  }

  /** $SHORTCUT `edge match {case source ~> target => f(source, target)}`.
    */
  val ~> = DiEdge
}
