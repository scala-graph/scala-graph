package scalax.collection.constrained

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.higherKinds
import scala.collection.Set
import scala.collection.mutable.{Map => MMap}

import scalax.collection.GraphPredef._
import PreCheckFollowUp._

sealed trait Op
sealed trait BinaryOp extends Op
case object And       extends BinaryOp
case object Or        extends BinaryOp

abstract class ConstraintOp[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G, val operator: Op)
    extends Constraint[N, E, G](self)

class ConstraintBinaryOp[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G,
                                                                     operator: BinaryOp,
                                                                     left: Constraint[N, E, G],
                                                                     right: Constraint[N, E, G])
    extends ConstraintOp[N, E, G](self, operator) {

  assert((self eq left.self) && (left.self eq right.self))

  protected class PreCheckResults(override val followUp: PreCheckFollowUp,
                                  constraint: Constraint[N, E, G],
                                  result: PreCheckResult)
      extends PreCheckResult(followUp) {
    val results: MMap[Constraint[N, E, G], PreCheckResult] = result match {
      case r: PreCheckResults => MMap(r.results.toSeq: _*)
      case _                  => MMap(constraint -> result)
    }
    def +=(op: Constraint[N, E, G], result: PreCheckResult) = {
      result match {
        case r: PreCheckResults => results ++= r.results
        case _                  => results += op -> result
      }
      this
    }
  }

  protected def eval(left: Constraint[N, E, G],
                     leftResult: PreCheckResult,
                     right: Constraint[N, E, G],
                     rightResult: => PreCheckResult): PreCheckResult = {
    var rightDone          = false
    val leftFollowUp       = leftResult.followUp
    lazy val rightFollowUp = { rightDone = true; rightResult.followUp }
    val followUp =
      operator match {
        case And if leftFollowUp != Abort => min(leftFollowUp, rightFollowUp)
        case And                          => Abort
        case Or if leftFollowUp != Abort  => leftFollowUp
        case Or                           => rightFollowUp
      }
    if (followUp == Abort) PreCheckResult(Abort)
    else {
      if (rightDone) {
        leftResult match {
          case r: PreCheckResults => r += (right, rightResult)
          case _                  => new PreCheckResults(min(leftFollowUp, rightFollowUp), left, leftResult) += (right, rightResult)
        }
      } else leftResult
    }
  }

  protected def eval[V <: ConstraintViolation](left: Either[V, G], right: => Either[V, G]): Either[V, G] =
    (left, right) match {
      case (success @ Right(_), Right(_)) => success
      case (failure @ Left(_), _)         => failure
      case (_, failure @ Left(_))         => failure
    }

  private type LNodeT = left.self.NodeT
  private type RNodeT = right.self.NodeT
  private type LEdgeT = left.self.EdgeT
  private type REdgeT = right.self.EdgeT

  final override def preCreate(nodes: collection.Traversable[N], edges: collection.Traversable[E[N]]): PreCheckResult =
    eval(left, left preCreate (nodes, edges), right, right preCreate (nodes, edges))

  final override def preAdd(node: N): PreCheckResult    = eval(left, left preAdd node, right, right preAdd node)
  final override def preAdd(edge: E[N]): PreCheckResult = eval(left, left preAdd edge, right, right preAdd edge)

  final override def preAdd(elems: InParam[N, E]*): PreCheckResult =
    eval(left, left preAdd (elems: _*), right, right preAdd (elems: _*))

  final override def preSubtract(node: self.NodeT, forced: Boolean): PreCheckResult =
    eval(
      left,
      left preSubtract (node.asInstanceOf[LNodeT], forced),
      right,
      right preSubtract (node.asInstanceOf[RNodeT], forced))

  final override def preSubtract(edge: self.EdgeT, simple: Boolean): PreCheckResult =
    eval(
      left,
      left preSubtract (edge.asInstanceOf[LEdgeT], simple),
      right,
      right preSubtract (edge.asInstanceOf[REdgeT], simple))

  final override def preSubtract(nodes: => Set[self.NodeT],
                                 edges: => Set[self.EdgeT],
                                 simple: Boolean): PreCheckResult =
    eval(
      left,
      left preSubtract (nodes.asInstanceOf[Set[LNodeT]], edges.asInstanceOf[Set[LEdgeT]], simple),
      right,
      right preSubtract (nodes.asInstanceOf[Set[RNodeT]], edges.asInstanceOf[Set[REdgeT]], simple)
    )

  final override def postAdd(newGraph: G @uV,
                             passedNodes: Traversable[N],
                             passedEdges: Traversable[E[N]],
                             preCheck: PreCheckResult): Either[PostCheckFailure, G] =
    eval(
      left postAdd (newGraph, passedNodes, passedEdges, preCheck),
      right postAdd (newGraph, passedNodes, passedEdges, preCheck)
    )

  final override def postSubtract(newGraph: G @uV,
                                  passedNodes: Traversable[N],
                                  passedEdges: Traversable[E[N]],
                                  preCheck: PreCheckResult): Either[PostCheckFailure, G] =
    eval(
      left postSubtract (newGraph, passedNodes, passedEdges, preCheck),
      right postSubtract (newGraph, passedNodes, passedEdges, preCheck))
}

/** Base class for any operation on `ConstraintCompanion`s.
  */
abstract class ConstraintCompanionOp(val operator: Op) extends ConstraintCompanion[Constraint]

/** Facilitates binary operations on `ConstraintCompanion`s.
  */
class ConstraintCompanionBinaryOp[CC[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]] <: Constraint[N, E, G]](
    operator: BinaryOp,
    left: ConstraintCompanion[CC],
    right: ConstraintCompanion[CC]
) extends ConstraintCompanionOp(operator) {

  def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) =
    new ConstraintBinaryOp[N, E, G](self, operator, left(self), right(self))
}
