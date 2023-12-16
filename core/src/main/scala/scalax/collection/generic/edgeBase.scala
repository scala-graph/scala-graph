package scalax.collection
package generic

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.annotation.{switch, tailrec}
import scala.collection.immutable.Iterable

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

  /** The endpoints of this edge, in other words the nodes this edge connects. */
  def ends: Several[N]

  /** The first node of this edge. */
  def node1: N

  /** The second node of this edge. */
  def node2: N

  /** The n'th node of this edge.
    * @throws IllegalArgumentException if `n` does not meet `0 <= n < arity`.
    */
  def node(n: Int): N

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
    *  In case of a directed hyperedge, a loop is given if any  of the targets is included in the sources.
    *  In case of an undirected hyperedge, a loop is given if any of the ends is a duplicate.
    */
  def isLooping: Boolean

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
  def sources: OneOrMore[N]

  /** All target ends of this edge. */
  def targets: OneOrMore[N]

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
}

object Edge {
  def unapply[N](e: Edge[N]): Some[Edge[Any]] = Some(e)
}

private[collection] trait InnerEdgeLike[+N] extends Edge[N]

/** Marker trait for companion objects of any non-labeled edge.
  */
sealed trait EdgeCompanionBase extends Serializable

/** Template for companion objects of generic edges.
  */
trait EdgeCompanion[+E[N] <: Edge[N]] extends EdgeCompanionBase {
  def apply[N](node_1: N, node_2: N): E[N]
  implicit def thisCompanion: this.type = this
}

trait AnyHyperEdge[+N] extends Edge[N] with EqHyper {

  def node1: N        = ends.head
  def node2: N        = ends(1)
  def node(n: Int): N = ends(n)
  def arity: Int      = ends.size

  def isDirected  = false
  def isHyperEdge = true

  def isLooping: Boolean = ends.iterator.toSet.size < arity

  override def sources: OneOrMore[N] = OneOrMore.fromUnsafe(ends.iterator)
  override def targets: OneOrMore[N] = sources

  override def isAt[M >: N](node: M): Boolean    = ends contains node
  override def isAt(pred: N => Boolean): Boolean = ends exists pred

  override def hasSource[M >: N](node: M): Boolean    = isAt(node)
  override def hasSource(pred: N => Boolean): Boolean = isAt(pred)

  override def hasTarget[M >: N](node: M): Boolean    = isAt(node)
  override def hasTarget(pred: N => Boolean): Boolean = isAt(pred)

  def withSources[U](f: N => U): Unit = sources.iterator foreach f
  def withTargets[U](f: N => U): Unit = targets.iterator foreach f

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
}

object AnyHyperEdge {
  def unapply[N](hE: AnyHyperEdge[N]): Some[Several[N]] = Some(hE.ends)
}

abstract class AbstractHyperEdge[+N](val ends: Several[N]) extends AnyHyperEdge[N]

object AbstractHyperEdge {
  def unapply[N](e: AbstractHyperEdge[N]): Some[Several[N]] = Some(e.ends)
}

abstract class AbstractGenericHyperEdge[+N, +CC[X] <: AbstractGenericHyperEdge[X, CC]](ends: Several[N])
    extends AbstractHyperEdge[N](ends)
    with GenericHyperEdgeMapper[CC]

abstract class AbstractUnlabeledGenericHyperEdge[+N, +CC[X] <: AbstractUnlabeledGenericHyperEdge[X, CC]](
    ends: Several[N]
) extends AbstractHyperEdge[N](ends)
    with GenericUnlabeledHyperEdgeMapper[CC]

/** The abstract methods of this trait must be implemented by companion objects of non-labeled hyperedges.
  */
trait HyperEdgeCompanion[+E[N] <: AbstractHyperEdge[N]] extends EdgeCompanionBase {

  def apply[N](ends: Several[N]): E[N]

  def apply[N](node_1: N, node_2: N, moreNodes: N*): E[N] = apply(new Several(node_1, node_2, moreNodes))

  final def unapply[N](edge: E[N] @uV): Some[Several[N]] = Some(edge.ends)

  /** `Some` hyperedge if `ends` contains at least two elements, otherwise `None`.
    */
  final def from[N](iterable: Iterable[N]): Option[E[N]] =
    Several.from(iterable) map (ends => apply(ends))

  /** A hyperedge with these `ends`.
    * @throws IllegalArgumentException if `ends` has not at least two elements.
    */
  final def fromUnsafe[N](iterable: Iterable[N]): E[N] =
    apply(Several.fromUnsafe(iterable))

  implicit def thisCompanion: this.type = this
}

trait AnyDiHyperEdge[+N] extends AnyHyperEdge[N] with EqDiHyper {

  override def node1: N         = sources.head
  override def ends: Several[N] = Several.fromUnsafe(sources.iterator ++ targets.iterator)

  override def arity: Int = sources.size + targets.size

  @inline final override def isDirected = true

  override def isLooping: Boolean = targets exists sources.contains

  override def hasSource[M >: N](node: M): Boolean    = sources exists (_ == node)
  override def hasSource(pred: N => Boolean): Boolean = sources exists pred

  override def hasTarget[M >: N](node: M): Boolean    = targets exists (_ == node)
  override def hasTarget(pred: N => Boolean): Boolean = targets exists pred

  override def matches[M >: N](n1: M, n2: M): Boolean               = sources.exists(_ == n1) && targets.exists(_ == n2)
  override def matches(p1: N => Boolean, p2: N => Boolean): Boolean = (sources exists p1) && (targets exists p2)
}

object AnyDiHyperEdge {
  def unapply[N](diH: AnyDiHyperEdge[N]): Some[(OneOrMore[N], OneOrMore[N])] = Some(diH.sources, diH.targets)
}

abstract class AbstractDiHyperEdge[+N](override val sources: OneOrMore[N], override val targets: OneOrMore[N])
    extends AnyDiHyperEdge[N]

object AbstractDiHyperEdge {
  def unapply[N](e: AbstractDiHyperEdge[N]): Some[(OneOrMore[N], OneOrMore[N])] = Some(e.sources, e.targets)
}

abstract class AbstractGenericDiHyperEdge[+N, +CC[X] <: AbstractGenericDiHyperEdge[X, CC]](
    sources: OneOrMore[N],
    targets: OneOrMore[N]
) extends AbstractDiHyperEdge[N](sources, targets)
    with GenericDiHyperEdgeMapper[CC]

abstract class AbstractGenericUnlabeledDiHyperEdge[+N, +CC[X] <: AbstractGenericUnlabeledDiHyperEdge[X, CC]](
    sources: OneOrMore[N],
    targets: OneOrMore[N]
) extends AbstractDiHyperEdge[N](sources, targets)
    with GenericUnlabeledDiHyperEdgeMapper[CC]

/** The abstract methods of this trait must be implemented by companion objects of directed, non-labeled hyperedges.
  */
trait DiHyperEdgeCompanion[+E[N] <: AbstractDiHyperEdge[N]] extends EdgeCompanionBase {

  def apply[N](sources: OneOrMore[N], targets: OneOrMore[N]): E[N]

  def apply[N](source_1: N, moreSources: N*)(target_1: N, moreTargets: N*): E[N] =
    apply(OneOrMore(source_1, moreSources: _*), OneOrMore(target_1, moreTargets: _*))

  def unapply[N](edge: E[N] @uV): Some[(OneOrMore[N], OneOrMore[N])] = Some(edge.sources, edge.targets)

  /** `Some` directed hyperedge if `sources` and `targets` have at least one element each, otherwise `None`.
    */
  final def from[N](sources: Iterable[N], targets: Iterable[N]): Option[E[N]] =
    (OneOrMore.from(sources), OneOrMore.from(targets)) match {
      case (Some(s), Some(t)) => Some(apply(s, t))
      case _                  => None
    }

  /** A directed hyperedge with the supplied `sources` and `targets`.
    * @throws IllegalArgumentException if `sources` or `targets` is empty.
    */
  final def fromUnsafe[N](sources: Iterable[N], targets: Iterable[N]): E[N] =
    apply(OneOrMore.fromUnsafe(sources), OneOrMore.fromUnsafe(targets))
}

trait AnyEdge[+N] extends Edge[N] {

  final override def node(n: Int): N = (n: @switch) match {
    case 0 => node1
    case 1 => node2
    case _ => throw new NoSuchElementException
  }

  @inline final override def arity: Int  = 2
  @inline final override def isHyperEdge = false

  def isLooping: Boolean = node1 == node2

  def ends: Several[N] = Several(node1, node2)

  @inline final override def isAt[M >: N](node: M): Boolean    = this.node1 == node || this.node2 == node
  @inline final override def isAt(pred: N => Boolean): Boolean = pred(this.node1) || pred(this.node2)
}

object AnyEdge {
  def unapply[N](e: AnyEdge[N]): Some[(N, N)] = Some(e.node1, e.node2)
}

trait AnyUnDiEdge[+N] extends AnyHyperEdge[N] with AnyEdge[N] with EqUnDi[N] {

  def source: N
  def target: N

  @inline final override def sources: OneOrMore[N] = OneOrMore(source, target)
  @inline final override def targets: OneOrMore[N] = sources

  @inline final override def node1: N = source
  @inline final override def node2: N = target

  final override def isLooping: Boolean = super[AnyEdge].isLooping

  final override def hasSource[M >: N](node: M): Boolean    = source == node || target == node
  final override def hasSource(pred: N => Boolean): Boolean = pred(source) || pred(target)

  final override def hasTarget[M >: N](node: M): Boolean    = hasSource(node)
  final override def hasTarget(pred: N => Boolean): Boolean = hasSource(pred)

  final override def withSources[U](f: N => U): Unit = { f(source); f(target) }
  final override def withTargets[U](f: N => U): Unit = withSources(f)

  override def isDirected = false

  override def matches[M >: N](n1: M, n2: M): Boolean = unDiBaseEquals(n1, n2)
  override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
    p1(this.node1) && p2(this.node2) ||
      p1(this.node2) && p2(this.node1)
}

object AnyUnDiEdge {
  def unapply[N](e: AnyUnDiEdge[N]) = Some(e)
}

abstract class AbstractUnDiEdge[+N](val source: N, val target: N) extends AnyUnDiEdge[N]

trait AbstractGenericUnDiEdge[+N, +CC[X] <: AbstractGenericUnDiEdge[X, CC]]
    extends AnyUnDiEdge[N]
    with GenericEdgeMapper[CC]

trait AbstractGenericUnlabeledUnDiEdge[+N, +CC[X] <: AbstractGenericUnlabeledUnDiEdge[X, CC]]
    extends AnyUnDiEdge[N]
    with GenericUnlabeledEdgeMapper[CC]

trait AnyDiEdge[+N] extends AnyDiHyperEdge[N] with AnyEdge[N] with EqDi[N] {

  final override def ends: Several[N] = super[AnyEdge].ends

  def source: N
  def target: N

  @inline final override def node1: N = source
  @inline final override def node2: N = target

  final override def isLooping: Boolean = super[AnyEdge].isLooping

  final override def sources: OneOrMore[N] = OneOrMore(source)
  final override def targets: OneOrMore[N] = OneOrMore(target)

  final override def hasSource[M >: N](node: M): Boolean    = source == node
  final override def hasSource(pred: N => Boolean): Boolean = pred(source)

  final override def hasTarget[M >: N](node: M): Boolean    = target == node
  final override def hasTarget(pred: N => Boolean): Boolean = pred(target)

  final override def withSources[U](f: N => U): Unit = f(source)
  final override def withTargets[U](f: N => U): Unit = f(target)

  final override def matches[M >: N](n1: M, n2: M): Boolean               = diBaseEquals(n1, n2)
  final override def matches(p1: N => Boolean, p2: N => Boolean): Boolean = p1(source) && p2(target)
}

abstract class AbstractDiEdge[+N](val source: N, val target: N) extends AnyDiEdge[N]

trait AbstractGenericDiEdge[+N, +CC[X] <: AbstractGenericDiEdge[X, CC]] extends AnyDiEdge[N] with GenericEdgeMapper[CC]

trait AbstractGenericUnlabeledDiEdge[+N, +CC[X] <: AbstractGenericUnlabeledDiEdge[X, CC]]
    extends AnyDiEdge[N]
    with GenericUnlabeledEdgeMapper[CC]

private[collection] object Abstract {

  abstract class HyperEdge[+N](ends: Several[N]) extends AnyHyperEdge[N]

  abstract class OrderedHyperEdge[+N](ends: Several[N]) extends AnyHyperEdge[N] with OrderedEndpoints

  abstract class DiHyperEdge[+N](sources: OneOrMore[N], targets: OneOrMore[N]) extends AnyDiHyperEdge[N]

  abstract class OrderedDiHyperEdge[+N](sources: OneOrMore[N], targets: OneOrMore[N])
      extends AnyDiHyperEdge[N]
      with OrderedEndpoints

  abstract class UnDiEdge[+N](node_1: N, node_2: N) extends AnyUnDiEdge[N]
  abstract class DiEdge[+N](source: N, target: N)   extends AnyDiEdge[N]
}
