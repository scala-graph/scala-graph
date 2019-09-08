package scalax.collection.constrained

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.higherKinds
import scala.collection.Set

import scalax.collection.GraphPredef._

/** Base trait for ordinary `Constraint` companion objects.
  */
trait ConstraintCompanion[+CC[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]] <: Constraint[N, E, G]] {
  thisCompanion =>

  /** Instantiates a user constraint. */
  def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G): CC[N, E, G]

  /** Creates a new constraint companion of the type `ConstraintCompanionBinaryOp`
    * the `apply` of which returns `ConstraintBinaryOp` with the `And` operator.
    */
  def &&(that: ConstraintCompanion[Constraint]) = new ConstraintCompanionBinaryOp(And, this, that)

  /** Creates a new constraint companion of the type `ConstraintCompanionBinaryOp`
    * the `apply` of which returns `ConstraintBinaryOp` with the `Or` operator.
    */
  def ||(that: ConstraintCompanion[Constraint]) = new ConstraintCompanionBinaryOp(Or, this, that)

  protected[constrained] class PrefixedConstraintCompanion(prefix: Option[String]) extends ConstraintCompanion[CC] {
    override val stringPrefix: Option[String]                                   = prefix
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G): CC[N, E, G] = thisCompanion[N, E, G](self)
  }

  /** The `stringPrefix` of constrained `Graph`s using `this` constraint will be replaced
    *  by this string unless `None`. */
  def stringPrefix: Option[String] = None

  /** Sets `stringPrefix` of constrained `Graph`s using `this` combined constraint companion
    *  to `graphStringPrefix`. */
  def withStringPrefix(stringPrefix: String): PrefixedConstraintCompanion = {
    val printable = stringPrefix.trim filter (_.isLetterOrDigit)
    new PrefixedConstraintCompanion(if (printable.length > 0) Some(printable) else None)
  }
}

/** Enumerates the possible return statuses (also: follow-up activity) of a pre-check:
  * `Abort` instructs the caller to cancel the operation because the pre-check failed;
  * `PostCheck` means that the post-check (commit) still must be called;
  * `Complete` means that the operation can safely be completed without invoking the post-check.
  */
object PreCheckFollowUp extends Enumeration {
  type PreCheckFollowUp = Value
  val Abort, PostCheck, Complete = Value

  /** The minimum of `a` and `b` treating `Abort` < `PostCheck` < `Complete`. */
  final def min(a: PreCheckFollowUp, b: PreCheckFollowUp): PreCheckFollowUp = if (a.id < b.id) a else b
}
import PreCheckFollowUp._

sealed trait ConstraintViolation

/** The return type of any pre-check. `followUp` contains the return status (follow-up
  *  activity). `Constraint` implementations are encouraged to extend this class to contain
  *  further data calculated in course of the pre-check and reusable in the following post-check.
  */
class PreCheckResult(val followUp: PreCheckFollowUp) extends ConstraintViolation {

  final def apply(): PreCheckFollowUp = followUp

  /** Whether `this.followUp` equals to `Abort`. */
  final def abort: Boolean = followUp == Abort

  /** Whether `this.followUp` equals to `PostCheck`. */
  final def postCheck: Boolean = followUp == PostCheck

  /** Whether `this.followUp` equals to `Complete`. */
  final def complete: Boolean = followUp == Complete

  /** Returns a tuple of `this` and `this.followUp`. */
  final def tupled: (PreCheckResult, PreCheckFollowUp) = this match { case PreCheckResult(r, f) => (r, f) }
}

trait PreCheckResultCompanion {
  def apply(followUp: PreCheckFollowUp): PreCheckResult

  /** If `ok` is `true` returns a new `PreCheckResult` with `followUp` equaling to `PostCheck`
    *  otherwise to `Abort`. */
  def postCheck(ok: Boolean): PreCheckResult = apply(if (ok) PostCheck else Abort)

  /** If `ok` is `true` returns a new `PreCheckResult` with `followUp` equaling to `Complete`
    *  otherwise to `Abort`. */
  def complete(ok: Boolean): PreCheckResult = apply(if (ok) Complete else Abort)
}

object PreCheckResult extends PreCheckResultCompanion {
  def apply(followUp: PreCheckFollowUp) = new PreCheckResult(followUp)
  def unapply(preCheck: PreCheckResult): Option[(PreCheckResult, PreCheckFollowUp)] =
    if (preCheck eq null) None else Some(preCheck, preCheck.followUp)
}

case class PostCheckFailure(cause: Any) extends ConstraintViolation

/** This template contains all methods that constrained graphs call
  * to decide whether operations altering a mutable graph or operations
  * yielding a new graph from an immutable or mutable graph are valid.
  *
  * Constraint methods are called on graph creation and node/edge addition and subtraction
  * at two points of time, respectively: prior to the operation and after the operation has
  * taken place but still may be rolled back. Thus,
  * constraint method names are prefixed by `pre` or `post`. Pre-ckecks return `Abort`,
  * `PostCheck` or `Complete` while post-checks return Boolean stating whether the operation
  * should be committed or rolled back. Pre-checks can inspect the operands only. In contrast,
  * post-checks additionally allow to inspect the would-be graph after the operation has taken place
  * but has not yet been committed.
  *
  * For performance reasons, implementations should prefer implementing pre-check methods.
  * If it's necessary to check not only the operands but the whole would-be graph,
  * the appropriate post-check methods should be overridden.
  *
  * @define SELFGRAPH Use `self` to access the associated graph.
  * @define PREPOST This pre-check may be omitted by letting it always return `postCheck`
  *         and overriding the corresponding post-check `commit*` method.
  * @define SELFCOMMIT For immutable graphs, `self` maintains the state before the
  *         addition but for mutable graphs, it is already mutated to the required state.
  * @define PRECHECKRET The results of the pre-check containing the follow-up activity
  *         and possibly any intermediate computation result to be used during the
  *         post-check. To add computation results `PreCheckResult` must be extended.
  * @author Peter Empen
  */
trait ConstraintMethods[N, E[X] <: EdgeLikeIn[X], +G <: Graph[N, E]] {

  /** When extending `Constraint`, `self` will denote the attached constrained graph.
    * The factory methods of the companion object `scalax.collection.constrained.Graph`
    * initialize `self` to the correct graph instance.
    * When extending `Constrained`, `self` will denote `this` graph.
    */
  val self: G

  /** This pre-check is called on constructing a graph through its companion object.
    * It must return whether the graph is allowed to be populated with `nodes` and `edges`.
    * The default implementation calls `preAdd` for each node and edge.
    *
    * Note that nodes and edges coming from node/edge input streams are not checked.
    * So when utilizing streams the post check `postAdd` must be served.
    *
    *  @param nodes the outer nodes the graph is to be populated with; nodes
    *         being edge ends may but need not be contained in `nodes`.
    *  @param edges the outer edges the graph is to be populated with.
    *  @return $PRECHECKRET
    */
  def preCreate(nodes: collection.Traversable[N], edges: collection.Traversable[E[N]]): PreCheckResult =
    PreCheckResult.postCheck(
      (nodes forall ((n: N) => !preAdd(n).abort)) &&
        (edges forall ((e: E[N]) => !preAdd(e).abort)))

  /** This pre-check must return `Abort` if the addition is to be canceled, `PostCheck` if `postAdd`
    * is to be called to decide or `Complete` if the outer `node` is allowed to be added.
    * If `postAdd` has been implemented, this method may always return `PostCheck`.
    * $PREPOST
    * $SELFGRAPH
    *
    * @param node to be added
    * @return $PRECHECKRET
    */
  def preAdd(node: N): PreCheckResult

  /** This pre-check must return `Abort` if the addition is to be canceled, `PostCheck` if `postAdd`
    * is to be called to decide or `Complete` if the outer `edge` is allowed to be added.
    * If `postAdd` has been implemented, this method may always return `PostCheck`.
    * $PREPOST
    * $SELFGRAPH
    *
    * @param edge to be added.
    * @return $PRECHECKRET
    */
  def preAdd(edge: E[N]): PreCheckResult

  /** This pre-check must return `Abort` if the addition of the outer nodes and/or edges in `elems`
    * is to be canceled, `PostCheck` if `postAdd` is to be called to decide or
    * `Complete` if the the outer nodes and/or edges are allowed to be added.
    * If `postAdd` has been implemented, this method may always return `PostCheck`.
    * The default implementation calls `preAdd(node)`/`preAdd(edge)` element-wise.
    * As for most cases this won't be satisfactory a domain-specific implementation should
    * be provided.
    * $SELFGRAPH
    *
    * @param elems nodes and/or edges to be added possibly containing duplicates.
    * @return $PRECHECKRET
    */
  def preAdd(elems: InParam[N, E]*): PreCheckResult =
    PreCheckResult.postCheck(elems forall {
      _ match {
        case node: OuterNode[N]    => !preAdd(node.value).abort
        case edge: OuterEdge[N, E] => !preAdd(edge.edge).abort
      }
    })

  /** This pre-check must return `Abort` if the subtraction of `node` is to be canceled,
    * `PostCheck` if `postSubtract` is to be called to decide or
    * `Complete` if the the `node` is allowed to be subtracted.
    * $PREPOST
    * $SELFGRAPH
    *
    * @param node the inner to be subtracted.
    * @param forced `true` for standard (ripple by `-`), `false` for gentle (by `-?`) removal.
    * @return $PRECHECKRET
    */
  def preSubtract(node: self.NodeT, forced: Boolean): PreCheckResult

  /** This pre-check must return `Abort` if the subtraction of `edge` is to be canceled,
    * `PostCheck` if `postSubtract` is to be called to decide or
    * `Complete` if the the `edge` is allowed to be subtracted.
    * $PREPOST
    * $SELFGRAPH
    *
    * @param edge the inner edge to be subtracted.
    * @param simple `true` for standard (edge-only by `-`),
    *               `false` for ripple (by `-!`) removal.
    * @return $PRECHECKRET
    */
  def preSubtract(edge: self.EdgeT, simple: Boolean): PreCheckResult

  /** This pre-check must return `Abort` if the subtraction of `nodes` and/or `edges`
    * is to be canceled, `PostCheck` if `postSubtract` is to be called to decide or
    * `Complete` if `nodes` and/or `edges` are allowed to be subtracted.
    * It is typically triggered by the `--` operation.
    * The default implementation element-wise calls `preSubtract(node, simple)` or
    * `preSubtract(edge, simple)`, respectively.
    * As for most cases this won't be satisfactory a domain-specific implementation
    * should be provided.
    * $SELFGRAPH
    *
    * @param nodes the inner nodes to be subtracted not necessarily including
    *              the ends of edges to be subtracted. Call allNodes to get the
    *              complete set of nodes to be subtracted.
    * @param edges the inner edges to be subtracted.
    * @param simple `true` for standard (edge-only by `-`),
    *               `false` for ripple (by `-!`) removal.
    * @return $PRECHECKRET
    */
  def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): PreCheckResult =
    PreCheckResult.postCheck(
      (nodes forall (n => !preSubtract(n, simple).abort)) &&
        (edges forall (e => !preSubtract(e, simple).abort)))

  /** This post-check must return whether `newGraph` should be committed or the add
    * operation is to be rolled back.
    * $SELFGRAPH
    * $SELFCOMMIT
    *
    * @param newGraph the after-addition would-be graph waiting for commit.
    * @param passedNodes the normalized nodes passed to the add operation.
    * @param passedEdges the normalized edges passed to the add operation.
    * @param preCheck the result of `preAdd`.
    * @return `None` to accept `newGraph` or `Some` reason for constraint violation resp. rejection
    */
  def postAdd(newGraph: G @uV,
              passedNodes: Traversable[N],
              passedEdges: Traversable[E[N]],
              preCheck: PreCheckResult): Either[PostCheckFailure, G] = Right(newGraph)

  /** This post-check must return whether `newGraph` should be committed or the subtraction
    * is to be rolled back.
    * $SELFGRAPH
    * $SELFCOMMIT
    *
    * @param newGraph the after-subtraction would-be graph waiting for commit.
    * @param passedNodes the normalized nodes passed to the subtraction operation.
    * @param passedEdges the normalized edges passed to the subtraction operation.
    * @param preCheck the result of `preSubtract`.
    * @return `None` to accept `newGraph` or `Some` reason for constraint violation resp. rejection
    */
  def postSubtract(newGraph: G @uV,
                   passedNodes: Traversable[N],
                   passedEdges: Traversable[E[N]],
                   preCheck: PreCheckResult): Either[PostCheckFailure, G] = Right(newGraph)

  /** Consolidates all outer nodes of the arguments by adding the edge ends
    *  of `passedEdges` to `passedNodes`. */
  protected def allNodes(passedNodes: Traversable[N], passedEdges: Traversable[E[N]]): Set[N] = {
    val nodes = collection.mutable.Set[N]() ++ passedNodes
    passedEdges foreach (nodes ++= _)
    nodes
  }

  protected def nodesToAdd(passedNodes: Traversable[N], passedEdges: Traversable[E[N]]): Set[N] =
    allNodes(passedNodes, passedEdges).filter(self.find(_).isEmpty)
}

/** Template to be mixed in by any constrained graph class.
  *
  * The user of the dynamically constrained class [[scalax.collection.constrained.Graph]]
  * or its mutable counterpart need not to be concerned about this trait because
  * it has been mixed in there. She only needs to pass the companion object for her `Constraint`
  * implementation.
  *
  * Implementors of statically constrained graph classes have to mix in this trait
  * in their constrained graph implementations.
  *
  * @see ConstraintMethods
  * @author Peter Empen
  */
trait Constrained[N, E[X] <: EdgeLikeIn[X], +G <: Graph[N, E]] extends ConstraintMethods[N, E, G]

/** Template to be implemented and passed to a dynamically constrained graph class
  * by the user. Note that mutable state will be lost on any operation yielding a
  * new graph. Thus it is essential to either design classes inheriting from `Constraint`
  * in a pure immutable manner or taking internally care of whether the state has been lost.
  *
  * @param self denotes the attached constrained graph.
  * @see ConstraintMethods
  * @author Peter Empen
  */
abstract class Constraint[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
    extends ConstraintMethods[N, E, G] {

  /** Creates a new constraint of the type `ConstraintBinaryOp` with pre- and post-check methods
    * each of which returning `true` if both `this`' ''and'' `that`'s corresponding
    * pre- and post-checks return `true`.
    */
  def &&(that: Constraint[N, E, G]) = new ConstraintBinaryOp[N, E, G](self, And, this, that)

  /** Creates a new constraint of the type `ConstraintBinaryOp` with pre- and post-check methods
    * each of which returning `true` if either `this`' ''or'' `other`'s corresponding
    * pre- and post-checks returns `true`.
    */
  def ||(that: Constraint[N, E, G]) = new ConstraintBinaryOp[N, E, G](self, Or, this, that)
}
