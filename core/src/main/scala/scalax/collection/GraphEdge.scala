package scalax.collection

import scala.annotation.{switch, tailrec}
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.AbstractIterable

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
  sealed trait EdgeLike[+N] extends Eq {
    import EdgeLike.ValidationException

    /** The endpoints of this edge, in other words the nodes this edge joins. */
    def ends: Iterable[N]

    /** The n'th node with 0 <= n < arity. */
    def _n(n: Int): N

    /** Number of the endpoints of this edge. At least two nodes are joined.
      * In case of a hook, the two nodes are identical.
      * Hyperedges may link more than two nodes.
      */
    def arity: Int

    /** Determines whether the `arity` is valid.
      * $CalledByValidate
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

    final protected def handleFailure(msgPrefix: String): Unit =
      throw new ValidationException(s"$msgPrefix at $toString.")

    /** Performs basic edge validation by calling `isValidArity` and `noNullEnd`.
      * `isValidCustom` is also called but you need to override this member to perform additional validation.
      * This validation method needs to be called in the constructor of any edge class
      * that directly extends or mixes in `EdgeLike`.
      *
      * @throws ValidationException if any of the basic validations or the additional custom validation fails.
      */
    final protected def validate(): Unit =
      if (!isValidArity) handleFailure("Invalid arity detected")
      else if (!noNullEnd) handleFailure("Null endpoint detected")
      else if (!isValidCustom) handleFailure(customMsgPrefix)

    /** Whether this edge is directed. */
    def isDirected: Boolean

    /** Whether this edge is undirected. */
    @inline final def isUndirected: Boolean = !isDirected

    /** Whether this edge's type is hyperedge meaning that it may have more than two ends. */
    def isHyperEdge: Boolean

    /** Whether this edge has exactly two ends. */
    @inline final def nonHyperEdge: Boolean = !isHyperEdge

    /** Whether this edge produces a self-loop.
      *  In case of a non-hyperedge, a loop is given if the incident nodes are equal.
      *  In case of a directed hyperedge, a loop is given if the source is equal to
      *  any of the targets.
      *  In case of an undirected hyperedge, a loop is given if any pair of incident
      *  nodes has equal nodes.
      */
    def isLooping: Boolean =
      if (arity == 2) ends.head == ends.drop(1).head
      else if (isDirected) ends.drop(1) exists (_ == ends.head)
      else (MSet() ++= ends).size < arity

    /** Same as `! looping`. */
    @inline final def nonLooping: Boolean = !isLooping

    /** The weight of this edge with a default of 1.
      *
      * Note that `weight` is normally not part of the edge key (hashCode). As a result, edges with different weights
      * connecting the same nodes will be evaluated as equal and thus added once and only once to the graph.
      * In case you need multi-edges based on different weights you should define a custom edge class
      * that mixes in `ExtendedKey` and adds `weight` to `keyAttributes`.
      */
    def weight: Double = 1

    def isLabeled: Boolean = false

    /** Same as `isAt`. */
    @inline final def contains[M >: N](node: M): Boolean = isAt(node)

    /** All source ends of this edge. */
    def sources: Iterable[N]

    /** All target ends of this edge. */
    def targets: Iterable[N]

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

    /** Applies `f` to all source ends of this edge without any memory allocation. */
    def withSources[U](f: N => U): Unit

    /** Applies `f` to the target ends of this edge without any memory allocation. */
    def withTargets[U](f: N => U): Unit

    /** `true` if
      *  a. being an undirected edge, both `n1` and `n2` are at this edge
      *  a. being a directed edge, `n1` is a source and `n2` a target of this edge.
      */
    def matches[M >: N](n1: M, n2: M): Boolean

    /** `true` if<br />
      *  a) two distinct ends of this undirected edge exist
      *     for which `p1` and `p2` hold or<br />
      *  b) `p1` holds for a source and `p2` for a target of this directed edge.
      */
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

    final protected def thisSimpleClassName = try this.getClass.getSimpleName
    catch { // Malformed class name
      case _: java.lang.InternalError => this.getClass.getName
    }
    def stringPrefix                           = "Nodes"
    protected def nodesToStringWithParenthesis = false
    protected def nodesToStringSeparator: String
    protected def nodesToString: String =
      if (nodesToStringWithParenthesis)
        ends.toString.patch(0, stringPrefix, stringPrefix.length)
      else
        ends mkString nodesToStringSeparator

    protected def attributesToString          = ""
    protected def toStringWithParenthesis     = false
    protected def brackets: EdgeLike.Brackets = EdgeLike.curlyBraces
    override def toString: String = {
      val attr          = attributesToString
      val woParenthesis = nodesToString + (if (attr.length > 0) attr else "")
      if (toStringWithParenthesis)
        thisSimpleClassName + brackets.left + woParenthesis + brackets.right
      else
        woParenthesis
    }
  }

  object EdgeLike {
    def unapply[N](e: EdgeLike[N]) = Some(e)

    protected val curlyBraces = Brackets('{', '}')

    case class Brackets(left: Char, right: Char)

    class ValidationException(val msg: String) extends Exception
  }

  private[collection] trait InnerEdgeLike[N] extends EdgeLike[N]

  protected[collection] trait Keyed

  /** Defines how to handle the ends of hyperedges, or the source/target ends of directed hyperedges,
    *  with respect to equality.
    */
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
      CollectionKind.from(duplicatesAllowed = true, orderSignificant = edge.isInstanceOf[OrderedEndpoints])

    def unapply(kind: CollectionKind): Option[(Boolean, Boolean)] =
      Some((kind.duplicatesAllowed, kind.orderSignificant))
  }

  /** Marks a hyperedge, $ORDIHYPER, to handle the endpoints
    *  as an unordered collection of nodes with duplicates allowed.
    */
  case object Bag extends CollectionKind(true, false)

  /** Marks a hyperedge, $ORDIHYPER, to handle the endpoints
    *  as an ordered collection of nodes with duplicates allowed.
    */
  case object Sequence extends CollectionKind(true, true)

  /** Marks (directed) hyperedge endpoints to have a significant order. */
  protected[collection] trait OrderedEndpoints

  sealed protected[collection] trait Eq extends Equals {
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
          val b = bs.next()
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
    def equalTargets(
        left: EdgeLike[_],
        leftEnds: Iterable[_],
        right: EdgeLike[_],
        rightEnds: Iterable[_],
        arity: Int
    ): Boolean = {
      val thisOrdered = left.isInstanceOf[OrderedEndpoints]
      val thatOrdered = right.isInstanceOf[OrderedEndpoints]
      thisOrdered == thatOrdered && (
        if (thisOrdered) leftEnds.toSeq == rightEnds.toSeq
        else Eq.nrEqualingNodes(leftEnds.iterator, rightEnds.toIterable) == arity
      )
    }
  }

  protected[collection] trait EqHyper extends Eq {
    this: AnyHyperEdge[_] =>
    override protected def baseEquals(other: EdgeLike[_]): Boolean = {
      val (thisArity, thatArity) = (arity, other.arity)
      if (thisArity == thatArity)
        Eq.equalTargets(this, this.sources, other, other.sources, thisArity)
      else false
    }
    override protected def baseHashCode: Int = ends.foldLeft(0)(_ ^ _.hashCode)
  }

  /** Equality for targets handled as a $BAG.
    *  Targets are equal if they contain the same nodes irrespective of their position.
    */
  protected[collection] trait EqDiHyper extends Eq {
    this: AnyDiHyperEdge[_] =>

    override protected def baseEquals(other: EdgeLike[_]): Boolean = {
      val (thisArity, thatArity) = (arity, other.arity)
      if (thisArity == thatArity)
        if (thisArity == 2)
          this.sources.head == other.sources.head &&
          this.targets.head == other.targets.head
        else
          other match {
            case diHyper: AnyDiHyperEdge[_] =>
              this.sources.head == diHyper.sources.head &&
              Eq.equalTargets(this, this.targets, other, other.targets, arity - 1)
            case _ => false
          }
      else false
    }

    override protected def baseHashCode: Int = {
      var m = 4

      def mul(i: Int): Int = { m += 3; m * i }
      ends.foldLeft(0)((s: Int, n: Any) => s ^ mul(n.hashCode))
    }
  }

  protected[collection] trait EqUnDi[+N] extends Eq {
    this: AnyUnDiEdge[N] =>

    @inline final protected def unDiBaseEquals(n1: Any, n2: Any): Boolean =
      this._1 == n1 && this._2 == n2 ||
        this._1 == n2 && this._2 == n1

    override protected def baseEquals(other: EdgeLike[_]): Boolean = other match {
      case edge: AnyEdge[_] => unDiBaseEquals(edge._1, edge._2)
      case hyper: AnyHyperEdge[_] if hyper.isUndirected && hyper.arity == 2 =>
        unDiBaseEquals(hyper._n(0), hyper._n(1))
      case _ => false
    }

    override protected def baseHashCode: Int = _1.## ^ _2.##
  }

  protected[collection] trait EqDi[+N] extends Eq {
    this: AnyDiEdge[N] =>

    @inline final protected def diBaseEquals(n1: Any, n2: Any): Boolean =
      this._1 == n1 &&
        this._2 == n2

    final override protected def baseEquals(other: EdgeLike[_]): Boolean = other match {
      case edge: AnyDiEdge[_]                           => diBaseEquals(edge._1, edge._2)
      case hyper: AnyDiHyperEdge[_] if hyper.arity == 2 => diBaseEquals(hyper.sources.head, hyper.targets.head)
      case _                                            => false
    }

    override protected def baseHashCode: Int = (23 * _1.##) ^ _2.##
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
  trait ExtendedKey[+N] { this: EdgeLike[N] =>

    /** Each element in this sequence references an attribute of the custom
      * edge which composes the key of this edge. All attributes added to this sequence
      * will be considered when calculating `equals` and `hashCode`.
      *
      * Neither an empty sequence, nor null elements are permitted.
      */
    def keyAttributes: Seq[Any]

    override def equals(other: Any): Boolean =
      super.equals(other) && (other match {
        case that: ExtendedKey[_] => this.keyAttributes == that.keyAttributes
        case _                    => false
      })

    override def hashCode: Int = super.hashCode + keyAttributes.map(_.## * 41).sum

    override protected def attributesToString: String
  }
  object ExtendedKey {
    def unapply[N](e: ExtendedKey[N]) = Some(e)
  }

  trait LoopFreeEdge[+N] { this: EdgeLike[N] =>
    override protected def isValidCustom: Boolean =
      if (arity == 2) ends.head != ends.drop(1).head
      else nonLooping
    override protected def customMsgPrefix = "Unexpected loop detected"
  }
  object LoopFreeEdge {
    def unapply[N](e: LoopFreeEdge[N]) = Some(e)
  }

  trait Label[L] { this: EdgeLike[_] =>
    def isLabeled: Boolean = true
    def label: L
  }

  object LEdge {
    def unapply[L](lEdge: Label[L]): Option[L] = Some(lEdge.label)
  }

  /** Marker trait for companion objects of any kind of edge.
    */
  sealed trait EdgeCompanionBase[+E[N] <: EdgeLike[N]] extends Serializable

  /** The abstract methods of this trait must be implemented by companion objects
    * of simple (non-weighted, non-labeled) edges.
    */
  trait EdgeCompanion[+E[N] <: EdgeLike[N]] extends EdgeCompanionBase[E] {
    def apply[NN](node_1: NN, node_2: NN): E[NN]
    implicit def thisCompanion: this.type = this
  }

  sealed protected[collection] trait Mapper
  sealed protected[collection] trait GenericMapper extends Mapper
  sealed protected[collection] trait PartialMapper extends Mapper

  sealed protected[collection] trait HyperEdgeMapper   extends Mapper
  sealed protected[collection] trait DiHyperEdgeMapper extends Mapper
  sealed protected[collection] trait EdgeMapper        extends Mapper

  sealed trait GenericHyperEdgeMapper   extends GenericMapper with HyperEdgeMapper
  sealed trait GenericDiHyperEdgeMapper extends GenericMapper with DiHyperEdgeMapper

  trait GenericEdgeMapper[+N, +This[X] <: AnyEdge[X]] extends GenericMapper with EdgeMapper {
    def map[NN](node_1: NN, node_2: NN): This[NN]
  }

  trait PartialHyperEdgeMapper[+N, +This <: AnyHyperEdge[N]] extends PartialMapper with HyperEdgeMapper {
    def map[NN]: PartialFunction[Iterable[NN], This]
  }
  trait PartialDiHyperEdgeMapper[+N, +This <: AnyDiHyperEdge[N]] extends PartialMapper with DiHyperEdgeMapper {
    def map[NN]: PartialFunction[(Iterable[NN], Iterable[NN]), This]
  }
  trait PartialEdgeMapper[+N, +This <: AnyEdge[N]] extends PartialMapper with EdgeMapper {
    def map[NN]: PartialFunction[(NN, NN), This]
  }

  trait AnyHyperEdge[+N] extends EdgeLike[N] with EqHyper {

    def _1: N         = ends.head
    def _2: N         = ends.drop(1).head
    def _n(n: Int): N = ends.drop(n).head
    def arity: Int    = ends.size

    def isDirected  = false
    def isHyperEdge = true

    override def sources: Iterable[N] = ends
    override def targets: Iterable[N] = sources

    protected def isValidArity: Boolean = ends.size >= 2
    protected def noNullEnd(coll: Iterable[N @uV]): Boolean = coll forall {
      case n: AnyRef => n ne null
      case _         => true
    }

    protected def noNullEnd: Boolean = noNullEnd(ends)

    override def isAt[M >: N](node: M): Boolean    = ends.iterator contains node
    override def isAt(pred: N => Boolean): Boolean = ends exists pred

    override def hasSource[M >: N](node: M): Boolean    = isAt(node)
    override def hasSource(pred: N => Boolean): Boolean = isAt(pred)

    override def hasTarget[M >: N](node: M): Boolean    = isAt(node)
    override def hasTarget(pred: N => Boolean): Boolean = isAt(pred)

    def withSources[U](f: N => U): Unit = sources foreach f
    def withTargets[U](f: N => U): Unit = targets foreach f

    final protected def matches(fList: List[N => Boolean]): Boolean = {
      val it = ends.iterator
      @tailrec def loop(checks: List[N => Boolean]): Boolean =
        if (checks.isEmpty) true
        else if (!it.hasNext) false
        else {
          val n = it.next()
          val f = checks find (f => f(n))
          if (f.isDefined) loop(checks diff List(f.get))
          else loop(checks)
        }
      loop(fList)
    }
    override def matches[M >: N](n1: M, n2: M): Boolean = matches(List((n: M) => n == n1, (n: M) => n == n2))
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean = matches(List(p1, p2))

    protected def nodesToStringSeparator: String = AnyHyperEdge.nodeSeparator
  }
  object AnyHyperEdge {
    def nodeSeparator: String = " ~ "
  }

  abstract class AbstractHyperEdge[+N](val ends: Iterable[N]) extends AnyHyperEdge[N]

  /** The abstract methods of this trait must be implemented by companion objects of simple
    * (non-weighted, non-labeled) hyperedges.
    */
  trait HyperEdgeCompanion[+E[N] <: AnyHyperEdge[N]] extends EdgeCompanionBase[E] {

    def apply[N](node_1: N, node_2: N, moreNodes: N*): E[N] = from(Iterable(node_1, node_2) ++ moreNodes)
    def apply[N](ends: Iterable[N]): E[N]                   = from(ends)
    def unapply[N](edge: E[N] @uV): Option[Seq[N]]          = Some(edge.ends.toSeq)

    protected def from[N](ends: Iterable[N]): E[N]
    implicit def thisCompanion: this.type = this
  }

  /** Represents an undirected hyperedge (hyperlink) with ends of bag semantic. */
  @SerialVersionUID(52)
  final case class HyperEdge[+N](override val ends: Iterable[N]) extends AnyHyperEdge[N] {
    validate()
  }
  object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
    protected def from[N](ends: Iterable[N]): HyperEdge[N] = new HyperEdge[N](ends)
  }

  /** $SHORTCUT `hyperedge match { case ~~(ends) => f(ends) }`. */
  val ~~ = HyperEdge

  @SerialVersionUID(-52)
  final case class OrderedHyperEdge[+N](override val ends: Iterable[N]) extends AnyHyperEdge[N] with OrderedEndpoints {
    validate()
  }
  object OrderedHyperEdge extends HyperEdgeCompanion[OrderedHyperEdge] {
    protected def from[N](ends: Iterable[N]): OrderedHyperEdge[N] = new OrderedHyperEdge[N](ends)
  }

  /** $SHORTCUT `hyperedge match { case ~~#(ends) => f(ends) }`. */
  val ~~# = OrderedHyperEdge

  trait AnyDiHyperEdge[+N] extends AnyHyperEdge[N] with EqDiHyper {

    override def _n(n: Int): N = ends.drop(n).head
    def ends: Iterable[N]      = sources ++ targets

    override def arity: Int                      = sources.size + targets.size
    override protected def isValidArity: Boolean = sources.iterator.hasNext && targets.iterator.hasNext
    override protected def noNullEnd: Boolean    = noNullEnd(sources) && noNullEnd(targets)

    @inline final override def isDirected = true

    override def hasSource[M >: N](node: M): Boolean    = sources exists (_ == node)
    override def hasSource(pred: N => Boolean): Boolean = sources exists pred

    override def hasTarget[M >: N](node: M): Boolean    = targets exists (_ == node)
    override def hasTarget(pred: N => Boolean): Boolean = targets exists pred

    override def matches[M >: N](n1: M, n2: M): Boolean = sources.exists(_ == n1) && targets.exists(_ == n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean = (sources exists p1) && (targets exists p2)

    override protected def nodesToStringSeparator: String = AnyDiHyperEdge.nodeSeparator
  }
  object AnyDiHyperEdge {
    def nodeSeparator: String = " ~~> "
  }

  abstract class AbstractDiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N])
      extends AnyDiHyperEdge[N]

  /** The abstract methods of this trait must be implemented by companion objects of directed, non-weighted, non-labeled hyperedges.
    */
  trait DiHyperEdgeCompanion[+E[N] <: AnyDiHyperEdge[N]] extends EdgeCompanionBase[E] {
    def apply[N](sources: Iterable[N], targets: Iterable[N]): E[N]
    def unapply[N](edge: E[N] @uV): Option[(Seq[N], Seq[N])] = Some(edge.sources.toSeq, edge.targets.toSeq)
  }

  /** Represents a directed edge in a hypergraph with an unlimited number of source and of target nodes.
    * Target nodes are handled as a $BAG.
    */
  @SerialVersionUID(53)
  final case class DiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N])
      extends AnyDiHyperEdge[N] {
    validate()
  }
  object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge]

  /** Represents a directed edge in a hypergraph with an unlimited number of source and of target nodes
    * where sources and targets are handled as an ordered sequence.
    */
  @SerialVersionUID(-53)
  final case class OrderedDiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N])
      extends AnyDiHyperEdge[N]
      with OrderedEndpoints {
    validate()
  }
  object OrderedDiHyperEdge extends DiHyperEdgeCompanion[OrderedDiHyperEdge]

  /** $SHORTCUT `diHyperedge match { case sources ~~> targets => f(sources, targets) }`. */
  val ~~> = DiHyperEdge

  trait AnyEdge[+N] extends EdgeLike[N] {

    def _1: N
    def _2: N

    final override def _n(n: Int): N = (n: @switch) match {
      case 0 => _1
      case 1 => _2
      case _ => throw new NoSuchElementException
    }

    // the following five methods should be made final as soon as DiEdge no more extends UnDiEdge
    @inline override def arity: Int             = 2
    @inline override protected def isValidArity = true

    @inline override def isHyperEdge = false

    override protected def noNullEnd: Boolean = {
      def notNull(n: N) = n match {
        case r: AnyRef => r ne null
        case _         => true
      }
      notNull(_1) && notNull(_2)
    }

    def ends: Iterable[N] = new AbstractIterable[N] {
      def iterator: Iterator[N] = Iterator.double(_1, _2)
    }

    override def isAt[M >: N](node: M): Boolean    = this._1 == node || this._2 == node
    override def isAt(pred: N => Boolean): Boolean = pred(this._1) || pred(this._2)
  }

  object AnyEdge {
    def unapply[N](e: AnyEdge[N]): Option[(N, N)] = if (e eq null) None else Some(e._1, e._2)
  }

  trait AnyUnDiEdge[+N] extends AnyHyperEdge[N] with AnyEdge[N] with EqUnDi[N] {

    def node_1: N
    def node_2: N

    @inline final override def sources: Set[N @uV] = Set(node_1, node_2)
    @inline final override def targets: Set[N @uV] = sources

    @inline final override def _1: N = node_1
    @inline final override def _2: N = node_2

    final override def hasSource[M >: N](node: M): Boolean    = node_1 == node || node_2 == node
    final override def hasSource(pred: N => Boolean): Boolean = pred(node_1) || pred(node_2)

    final override def hasTarget[M >: N](node: M): Boolean    = hasSource(node)
    final override def hasTarget(pred: N => Boolean): Boolean = hasSource(pred)

    final override def withSources[U](f: N => U): Unit = { f(node_1); f(node_2) }
    final override def withTargets[U](f: N => U): Unit = withSources(f)

    override def isDirected = false

    override def matches[M >: N](n1: M, n2: M): Boolean = unDiBaseEquals(n1, n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      p1(this._1) && p2(this._2) ||
        p1(this._2) && p2(this._1)

    override protected def nodesToStringSeparator: String = AnyUnDiEdge.nodeSeparator
  }
  object AnyUnDiEdge {
    val nodeSeparator                 = " ~ "
    def unapply[N](e: AnyUnDiEdge[N]) = Some(e)
  }

  abstract class AbstractUnDiEdge[+N](val node_1: N, val node_2: N) extends AnyUnDiEdge[N]

  trait AbstractGenericUnDiEdge[+N, +This[X] <: AbstractGenericUnDiEdge[X, This]]
      extends AnyHyperEdge[N]
      with AnyUnDiEdge[N]
      with EqUnDi[N]
      with GenericEdgeMapper[N, This]

  /** Represents a generic unlabeled undirected edge. */
  @SerialVersionUID(54)
  final case class UnDiEdge[+N](node_1: N, node_2: N) extends AbstractGenericUnDiEdge[N, UnDiEdge] {
    validate()
    def map[NN](node_1: NN, node_2: NN): UnDiEdge[NN] = copy[NN](node_1, node_2)
  }

  /** Factory for undirected edges.
    * `GraphPredef` also supports implicit conversion from `node_1 ~ node_2` to `UnDiEdge`.
    */
  object UnDiEdge extends EdgeCompanion[UnDiEdge]

  /** $SHORTCUT `edge match { case n1 ~ n2 => f(n1, n2) }`. */
  val ~ = UnDiEdge

  trait AnyDiEdge[+N] extends AnyDiHyperEdge[N] with AnyEdge[N] with EqDi[N] {

    final override def ends: Iterable[N] = super[AnyEdge].ends

    def source: N
    def target: N

    @inline final override def _1: N = source
    @inline final override def _2: N = target

    final override def sources: Set[N @uV] = Set(source)
    final override def targets: Set[N @uV] = Set(target)

    final override def hasSource[M >: N](node: M): Boolean    = source == node
    final override def hasSource(pred: N => Boolean): Boolean = pred(source)

    final override def hasTarget[M >: N](node: M): Boolean    = target == node
    final override def hasTarget(pred: N => Boolean): Boolean = pred(target)

    final override def withSources[U](f: N => U): Unit = f(source)
    final override def withTargets[U](f: N => U): Unit = f(target)

    final override def matches[M >: N](n1: M, n2: M): Boolean               = diBaseEquals(n1, n2)
    final override def matches(p1: N => Boolean, p2: N => Boolean): Boolean = p1(source) && p2(target)

    override protected def nodesToStringSeparator: String = AnyDiEdge.nodeSeparator
  }

  object AnyDiEdge {
    val nodeSeparator               = " ~> "
    def unapply[N](e: AnyDiEdge[N]) = Some(e)
  }

  abstract class AbstractDiEdge[+N](val source: N, val target: N) extends AnyDiEdge[N]

  trait AbstractGenericDiEdge[+N, +This[X] <: AbstractGenericDiEdge[X, This]]
      extends AnyDiHyperEdge[N]
      with AnyDiEdge[N]
      with EqDi[N]
      with GenericEdgeMapper[N, This]

  /** Represents a generic unlabeled directed edge. */
  @SerialVersionUID(55)
  final case class DiEdge[+N](source: N, target: N) extends AbstractGenericDiEdge[N, DiEdge] {
    validate()
    def map[NN](node_1: NN, node_2: NN): DiEdge[NN] = copy[NN](node_1, node_2)
  }

  /** Factory for directed edges. */
  object DiEdge extends EdgeCompanion[DiEdge]

  /** $SHORTCUT `edge match { case source ~> target => f(source, target) }`. */
  val ~> = DiEdge

  private[collection] object Abstract {

    abstract class HyperEdge[+N](ends: Iterable[N]) extends AnyHyperEdge[N]

    abstract class OrderedHyperEdge[+N](ends: Iterable[N]) extends AnyHyperEdge[N] with OrderedEndpoints

    abstract class DiHyperEdge[+N](sources: Iterable[N], targets: Iterable[N]) extends AnyDiHyperEdge[N]

    abstract class OrderedDiHyperEdge[+N](sources: Iterable[N], targets: Iterable[N])
        extends AnyDiHyperEdge[N]
        with OrderedEndpoints

    abstract class UnDiEdge[+N](node_1: N, node_2: N) extends AnyUnDiEdge[N]
    abstract class DiEdge[+N](source: N, target: N)   extends AnyDiEdge[N]
  }
}
