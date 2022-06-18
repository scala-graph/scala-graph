package scalax.collection.generic

import scalax.collection.{Iterable$Enrichments, MSet}

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.annotation.{switch, tailrec}
import scala.collection.AbstractIterable

/** Base template for all edges in a `Graph`.
  *
  * Library-provided edges are immutable for both mutable and immutable `Graph`.
  * However, when using mutable graphs with labeled edges where labels are mutable with respect to your use case,
  * you might want to make your label mutable accordingly. Otherwise a much less efficient edge replacement
  * would be necessary.
  *
  * @tparam N the type of the nodes (ends) of this edge.
  * @define CalledByValidate This function is called on every edge-instantiation
  *         by `validate` that throws EdgeException if this method returns `false`.
  * @define ISAT In case this edge is undirected this method maps to `isAt`
  * @author Peter Empen
  */
sealed trait Edge[+N] extends Equals {

  /** The endpoints of this edge, in other words the nodes this edge joins. */
  def ends: Iterable[N]

  /** The n'th node with 0 <= n < arity. */
  def _n(n: Int): N

  /** Number of the endpoints of this edge. At least two nodes are joined.
    * In case of a hook, the two nodes are identical.
    * Hyperedges may link more than two nodes.
    */
  def arity: Int

  protected def customMsgPrefix: String = "Custom validation failed"

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

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Edge[_]]

  final protected def thisSimpleClassName: String =
    try this.getClass.getSimpleName
    catch { // Malformed class name
      case _: java.lang.InternalError => this.getClass.getName
    }
  protected def nodesToStringWithParenthesis = false
  protected def nodesToStringSeparator: String
  protected def nodesToString: String =
    if (nodesToStringWithParenthesis) {
      def prefix = "Nodes"
      ends.toString.patch(0, prefix, prefix.length)
    } else
      ends mkString nodesToStringSeparator

  protected def toStringPostfix         = ""
  protected def toStringWithParenthesis = false
  protected def brackets: Edge.Brackets = Edge.curlyBraces

  /** Implementation based on protected methods that you can overwrite in your custom `Edge` class.
    */
  override def toString: String = {
    val postfix       = toStringPostfix
    val woParenthesis = nodesToString + (if (postfix.nonEmpty) postfix else "")
    if (toStringWithParenthesis)
      thisSimpleClassName + brackets.left + woParenthesis + brackets.right
    else
      woParenthesis
  }
}

object Edge {
  def unapply[N](e: Edge[N]): Option[Edge[Any]] = Some(e)

  protected val curlyBraces: Brackets = Brackets('{', '}')

  case class Brackets(left: Char, right: Char)
}

private[collection] trait InnerEdgeLike[+N] extends Edge[N]

/** Marker trait for companion objects of any non-labeled edge.
  */
sealed trait EdgeCompanionBase extends Serializable

/** The abstract methods of this trait must be implemented by companion objects of non-labeled edges.
  */
trait EdgeCompanion[+E[N] <: Edge[N]] extends EdgeCompanionBase {
  def apply[NN](node_1: NN, node_2: NN): E[NN]
  implicit def thisCompanion: this.type = this
}

trait AnyHyperEdge[+N] extends Edge[N] with EqHyper {

  def _1: N         = ends.head
  def _2: N         = ends.drop(1).head
  def _n(n: Int): N = ends.drop(n).head
  def arity: Int    = ends.size

  def isDirected  = false
  def isHyperEdge = true

  override def sources: Iterable[N] = ends
  override def targets: Iterable[N] = sources

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

abstract class AbstractHyperEdge[+N](val ends: Iterable[N]) extends AnyHyperEdge[N] {
  private def hasAtLeastTwoEnds: Boolean = {
    val it = ends.iterator
    it.hasNext && {
      it.next()
      it.hasNext
    }
  }
  require(hasAtLeastTwoEnds, "'ends' must have size >= 2")
}

/** The abstract methods of this trait must be implemented by companion objects of non-labeled hyperedges.
  */
trait HyperEdgeCompanion[+E[N] <: AnyHyperEdge[N]] extends EdgeCompanionBase {

  def apply[N](node_1: N, node_2: N, moreNodes: N*): E[N] = from(Iterable(node_1, node_2) ++ moreNodes)
  def apply[N](ends: Iterable[N]): E[N]                   = from(ends)
  def unapply[N](edge: E[N] @uV): Option[Seq[N]]          = Some(edge.ends.toSeq)

  protected def from[N](ends: Iterable[N]): E[N]
  implicit def thisCompanion: this.type = this
}

trait AnyDiHyperEdge[+N] extends AnyHyperEdge[N] with EqDiHyper {

  override def _n(n: Int): N = ends.drop(n).head
  def ends: Iterable[N]      = sources ++ targets

  override def arity: Int = sources.size + targets.size

  @inline final override def isDirected = true

  override def hasSource[M >: N](node: M): Boolean    = sources exists (_ == node)
  override def hasSource(pred: N => Boolean): Boolean = sources exists pred

  override def hasTarget[M >: N](node: M): Boolean    = targets exists (_ == node)
  override def hasTarget(pred: N => Boolean): Boolean = targets exists pred

  override def matches[M >: N](n1: M, n2: M): Boolean               = sources.exists(_ == n1) && targets.exists(_ == n2)
  override def matches(p1: N => Boolean, p2: N => Boolean): Boolean = (sources exists p1) && (targets exists p2)

  override protected def nodesToStringSeparator: String = AnyDiHyperEdge.nodeSeparator
}
object AnyDiHyperEdge {
  def nodeSeparator: String = " ~~> "
}

abstract class AbstractDiHyperEdge[+N](override val sources: Iterable[N], override val targets: Iterable[N])
    extends AnyDiHyperEdge[N] {
  require(sources.iterator.nonEmpty, "'sources' must not be empty")
  require(targets.iterator.nonEmpty, "'targets' must not be empty")
}

/** The abstract methods of this trait must be implemented by companion objects of directed, non-labeled hyperedges.
  */
trait DiHyperEdgeCompanion[+E[N] <: AnyDiHyperEdge[N]] extends EdgeCompanionBase {
  def apply[N](sources: Iterable[N], targets: Iterable[N]): E[N]
  def unapply[N](edge: E[N] @uV): Option[(Seq[N], Seq[N])] = Some(edge.sources.toSeq, edge.targets.toSeq)
}

trait AnyEdge[+N] extends Edge[N] { this: Eq =>

  def _1: N
  def _2: N

  final override def _n(n: Int): N = (n: @switch) match {
    case 0 => _1
    case 1 => _2
    case _ => throw new NoSuchElementException
  }

  // the following five methods should be made final as soon as DiEdge no more extends UnDiEdge
  @inline override def arity: Int  = 2
  @inline override def isHyperEdge = false

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

  def source: N
  def target: N

  @inline final override def sources: Set[N @uV] = Set(source, target)
  @inline final override def targets: Set[N @uV] = sources

  @inline final override def _1: N = source
  @inline final override def _2: N = target

  final override def hasSource[M >: N](node: M): Boolean    = source == node || target == node
  final override def hasSource(pred: N => Boolean): Boolean = pred(source) || pred(target)

  final override def hasTarget[M >: N](node: M): Boolean    = hasSource(node)
  final override def hasTarget(pred: N => Boolean): Boolean = hasSource(pred)

  final override def withSources[U](f: N => U): Unit = { f(source); f(target) }
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

abstract class AbstractUnDiEdge[+N](val source: N, val target: N) extends AnyUnDiEdge[N]

trait AbstractGenericUnDiEdge[+N, +This[X] <: AbstractGenericUnDiEdge[X, This]]
    extends AnyHyperEdge[N]
    with AnyUnDiEdge[N]
    with EqUnDi[N]
    with GenericEdgeMapper[N, This]

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
